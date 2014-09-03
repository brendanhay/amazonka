{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CancelBundleTask
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
module Network.AWS.EC2.V2014_06_15.CancelBundleTask
    (
    -- * Request
      CancelBundleTask
    -- ** Request constructor
    , cancelBundleTask
    -- ** Request lenses
    , cbtrBundleId

    -- * Response
    , CancelBundleTaskResponse
    -- ** Response lenses
    , cbtsBundleTask
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CancelBundleTask' request.
cancelBundleTask :: Text -- ^ 'cbtrBundleId'
                 -> CancelBundleTask
cancelBundleTask p1 = CancelBundleTask
    { _cbtrBundleId = p1
    }

data CancelBundleTask = CancelBundleTask
    { _cbtrBundleId :: Text
      -- ^ The ID of the bundle task.
    } deriving (Show, Generic)

-- | The ID of the bundle task.
cbtrBundleId
    :: Functor f
    => (Text
    -> f (Text))
    -> CancelBundleTask
    -> f CancelBundleTask
cbtrBundleId f x =
    (\y -> x { _cbtrBundleId = y })
       <$> f (_cbtrBundleId x)
{-# INLINE cbtrBundleId #-}

instance ToQuery CancelBundleTask where
    toQuery = genericQuery def

data CancelBundleTaskResponse = CancelBundleTaskResponse
    { _cbtsBundleTask :: Maybe BundleTask
      -- ^ The bundle task.
    } deriving (Show, Generic)

-- | The bundle task.
cbtsBundleTask
    :: Functor f
    => (Maybe BundleTask
    -> f (Maybe BundleTask))
    -> CancelBundleTaskResponse
    -> f CancelBundleTaskResponse
cbtsBundleTask f x =
    (\y -> x { _cbtsBundleTask = y })
       <$> f (_cbtsBundleTask x)
{-# INLINE cbtsBundleTask #-}

instance FromXML CancelBundleTaskResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CancelBundleTask where
    type Sv CancelBundleTask = EC2
    type Rs CancelBundleTask = CancelBundleTaskResponse

    request = post "CancelBundleTask"
    response _ = xmlResponse
