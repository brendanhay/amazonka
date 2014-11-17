{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.PutRetentionPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the retention of the specified log group. A retention policy allows
-- you to configure the number of days you want to retain log events in the
-- specified log group.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutRetentionPolicy.html>
module Network.AWS.CloudWatchLogs.PutRetentionPolicy
    (
    -- * Request
      PutRetentionPolicy
    -- ** Request constructor
    , putRetentionPolicy
    -- ** Request lenses
    , prpLogGroupName
    , prpRetentionInDays

    -- * Response
    , PutRetentionPolicyResponse
    -- ** Response constructor
    , putRetentionPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudWatchLogs.Types
import qualified GHC.Exts

data PutRetentionPolicy = PutRetentionPolicy
    { _prpLogGroupName    :: Text
    , _prpRetentionInDays :: Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutRetentionPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prpLogGroupName' @::@ 'Text'
--
-- * 'prpRetentionInDays' @::@ 'Int'
--
putRetentionPolicy :: Text -- ^ 'prpLogGroupName'
                   -> Int -- ^ 'prpRetentionInDays'
                   -> PutRetentionPolicy
putRetentionPolicy p1 p2 = PutRetentionPolicy
    { _prpLogGroupName    = p1
    , _prpRetentionInDays = p2
    }

prpLogGroupName :: Lens' PutRetentionPolicy Text
prpLogGroupName = lens _prpLogGroupName (\s a -> s { _prpLogGroupName = a })

prpRetentionInDays :: Lens' PutRetentionPolicy Int
prpRetentionInDays =
    lens _prpRetentionInDays (\s a -> s { _prpRetentionInDays = a })

data PutRetentionPolicyResponse = PutRetentionPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutRetentionPolicyResponse' constructor.
putRetentionPolicyResponse :: PutRetentionPolicyResponse
putRetentionPolicyResponse = PutRetentionPolicyResponse

instance AWSRequest PutRetentionPolicy where
    type Sv PutRetentionPolicy = CloudWatchLogs
    type Rs PutRetentionPolicy = PutRetentionPolicyResponse

    request  = post
    response = nullResponse PutRetentionPolicyResponse

instance ToPath PutRetentionPolicy where
    toPath = const "/"

instance ToHeaders PutRetentionPolicy

instance ToQuery PutRetentionPolicy where
    toQuery = const mempty

instance ToJSON PutRetentionPolicy where
    toJSON = genericToJSON jsonOptions
