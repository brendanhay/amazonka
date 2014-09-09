{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.BundleInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Bundles an Amazon instance store-backed Windows instance. During bundling,
-- only the root device volume (C:\) is bundled. Data on other instance store
-- volumes is not preserved. This procedure is not applicable for Linux/Unix
-- instances or Windows instances that are backed by Amazon EBS. For more
-- information, see Creating an Instance Store-Backed Windows AMI. Example
-- This example request bundles the specified instance. Before you specify a
-- value for your access key ID, review and follow the guidance in Best
-- Practices for Managing AWS Access Keys.
-- https://ec2.amazonaws.com/?Action=BundleInstance &amp;InstanceId=i-e468cd8d
-- &amp;Storage.S3.AWSAccessKeyId='AKIAIOSFODNN7EXAMPLE'
-- &amp;Storage.S3.Bucket=myawsbucket &amp;Storage.S3.Prefix=winami
-- &amp;Storage.S3.UploadPolicy=eyJleHBpcmF0aW9uIjogIjIwMDgtMDgtMzBUMDg6NDk6MD
-- laIiwiY29uZGl0aW9ucyI6IFt7ImJ1Y2tldCI6ICJteS1idWNrZXQifSxbInN0YXJ0cy13aXRoIiwgI
-- iRrZXkiLCAibXktbmV3LWltYWdlIl0seyJhY2wiOiAiZWMyLWJ1bmRsZS1yZWFkIn1dfEXAMPLE
-- &amp;Storage.S3.UploadPolicySignature=fh5tyyyQD8W4COEthj3nlGNEXAMPLE
-- &amp;AUTHPARAMS &lt;BundleInstanceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;bundleInstanceTask&gt; &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;bundleId&gt;bun-c1a540a8&lt;/bundleId&gt;
-- &lt;state&gt;bundling&lt;/state&gt;
-- &lt;startTime&gt;2008-10-07T11:41:50.000Z&lt;/startTime&gt;
-- &lt;updateTime&gt;2008-10-07T11:51:50.000Z&lt;/updateTime&gt;
-- &lt;progress&gt;70%&lt;/progress&gt; &lt;storage&gt; &lt;S3&gt;
-- &lt;bucket&gt;myawsbucket&lt;/bucket&gt;
-- &lt;prefix&gt;winami&lt;/prefix&gt; &lt;/S3&gt; &lt;/storage&gt;
-- &lt;/bundleInstanceTask&gt; &lt;/BundleInstanceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.BundleInstance
    (
    -- * Request
      BundleInstance
    -- ** Request constructor
    , mkBundleInstance
    -- ** Request lenses
    , biInstanceId
    , biStorage

    -- * Response
    , BundleInstanceResponse
    -- ** Response constructor
    , mkBundleInstanceResponse
    -- ** Response lenses
    , birBundleTask
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data BundleInstance = BundleInstance
    { _biInstanceId :: Text
    , _biStorage :: Storage
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'BundleInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @Storage ::@ @Storage@
--
mkBundleInstance :: Text -- ^ 'biInstanceId'
                 -> Storage -- ^ 'biStorage'
                 -> BundleInstance
mkBundleInstance p1 p2 = BundleInstance
    { _biInstanceId = p1
    , _biStorage = p2
    }

-- | The ID of the instance to bundle.
biInstanceId :: Lens' BundleInstance Text
biInstanceId = lens _biInstanceId (\s a -> s { _biInstanceId = a })

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If you
-- specify a bucket that belongs to someone else, Amazon EC2 returns an error.
biStorage :: Lens' BundleInstance Storage
biStorage = lens _biStorage (\s a -> s { _biStorage = a })

instance ToQuery BundleInstance where
    toQuery = genericQuery def

newtype BundleInstanceResponse = BundleInstanceResponse
    { _birBundleTask :: Maybe BundleTask
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'BundleInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @BundleTask ::@ @Maybe BundleTask@
--
mkBundleInstanceResponse :: BundleInstanceResponse
mkBundleInstanceResponse = BundleInstanceResponse
    { _birBundleTask = Nothing
    }

-- | Information about the bundle task.
birBundleTask :: Lens' BundleInstanceResponse (Maybe BundleTask)
birBundleTask = lens _birBundleTask (\s a -> s { _birBundleTask = a })

instance FromXML BundleInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest BundleInstance where
    type Sv BundleInstance = EC2
    type Rs BundleInstance = BundleInstanceResponse

    request = post "BundleInstance"
    response _ = xmlResponse
