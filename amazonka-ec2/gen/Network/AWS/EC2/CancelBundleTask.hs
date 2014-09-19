{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelBundleTask
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels a bundling operation for an instance store-backed Windows instance.
-- Example This example request cancels the specified bundle task.
-- https://ec2.amazonaws.com/?Action=CancelBundleTask
-- &amp;BundleId=bun-cla322b9 &amp;AUTHPARAMS &lt;CancelBundleTaskResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;bundleInstanceTask&gt; &lt;instanceId&gt;i-12345678&lt;/instanceId&gt;
-- &lt;bundleId&gt;bun-cla322b9&lt;/bundleId&gt;
-- &lt;state&gt;canceling&lt;/state&gt;
-- &lt;startTime&gt;2008-10-07T11:41:50.000Z&lt;/startTime&gt;
-- &lt;updateTime&gt;2008-10-07T11:51:50.000Z&lt;/updateTime&gt;
-- &lt;progress&gt;20%&lt;/progress&gt; &lt;storage&gt; &lt;S3&gt;
-- &lt;bucket&gt;myawsbucket&lt;/bucket&gt;
-- &lt;prefix&gt;my-new-image&lt;/prefix&gt; &lt;/S3&gt; &lt;/storage&gt;
-- &lt;/bundleInstanceTask&gt; &lt;/CancelBundleTaskResponse&gt;.
module Network.AWS.EC2.CancelBundleTask
    (
    -- * Request
      CancelBundleTask
    -- ** Request constructor
    , cancelBundleTask
    -- ** Request lenses
    , cbtBundleId

    -- * Response
    , CancelBundleTaskResponse
    -- ** Response constructor
    , cancelBundleTaskResponse
    -- ** Response lenses
    , cbtrBundleTask
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype CancelBundleTask = CancelBundleTask
    { _cbtBundleId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelBundleTask' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @BundleId ::@ @Text@
--
cancelBundleTask :: Text -- ^ 'cbtBundleId'
                 -> CancelBundleTask
cancelBundleTask p1 = CancelBundleTask
    { _cbtBundleId = p1
    }

-- | The ID of the bundle task.
cbtBundleId :: Lens' CancelBundleTask Text
cbtBundleId = lens _cbtBundleId (\s a -> s { _cbtBundleId = a })

instance ToQuery CancelBundleTask where
    toQuery = genericQuery def

newtype CancelBundleTaskResponse = CancelBundleTaskResponse
    { _cbtrBundleTask :: Maybe BundleTask
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelBundleTaskResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @BundleTask ::@ @Maybe BundleTask@
--
cancelBundleTaskResponse :: CancelBundleTaskResponse
cancelBundleTaskResponse = CancelBundleTaskResponse
    { _cbtrBundleTask = Nothing
    }

-- | The bundle task.
cbtrBundleTask :: Lens' CancelBundleTaskResponse (Maybe BundleTask)
cbtrBundleTask = lens _cbtrBundleTask (\s a -> s { _cbtrBundleTask = a })

instance FromXML CancelBundleTaskResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CancelBundleTask where
    type Sv CancelBundleTask = EC2
    type Rs CancelBundleTask = CancelBundleTaskResponse

    request = post "CancelBundleTask"
    response _ = xmlResponse
