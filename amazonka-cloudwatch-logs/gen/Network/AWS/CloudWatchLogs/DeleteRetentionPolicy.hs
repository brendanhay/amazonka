{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the retention policy of the specified log group. Log events would
-- not expire if they belong to log groups without a retention policy.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteRetentionPolicy.html>
module Network.AWS.CloudWatchLogs.DeleteRetentionPolicy
    (
    -- * Request
      DeleteRetentionPolicy
    -- ** Request constructor
    , deleteRetentionPolicy
    -- ** Request lenses
    , drpLogGroupName

    -- * Response
    , DeleteRetentionPolicyResponse
    -- ** Response constructor
    , deleteRetentionPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudWatchLogs.Types
import qualified GHC.Exts

newtype DeleteRetentionPolicy = DeleteRetentionPolicy
    { _drpLogGroupName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteRetentionPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drpLogGroupName' @::@ 'Text'
--
deleteRetentionPolicy :: Text -- ^ 'drpLogGroupName'
                      -> DeleteRetentionPolicy
deleteRetentionPolicy p1 = DeleteRetentionPolicy
    { _drpLogGroupName = p1
    }

drpLogGroupName :: Lens' DeleteRetentionPolicy Text
drpLogGroupName = lens _drpLogGroupName (\s a -> s { _drpLogGroupName = a })

data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteRetentionPolicyResponse' constructor.
deleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse
deleteRetentionPolicyResponse = DeleteRetentionPolicyResponse

instance ToPath DeleteRetentionPolicy where
    toPath = const "/"

instance ToQuery DeleteRetentionPolicy where
    toQuery = const mempty

instance ToHeaders DeleteRetentionPolicy

instance ToJSON DeleteRetentionPolicy where
    toJSON DeleteRetentionPolicy{..} = object
        [ "logGroupName" .= _drpLogGroupName
        ]

json

instance AWSRequest DeleteRetentionPolicy where
    type Sv DeleteRetentionPolicy = CloudWatchLogs
    type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse

    request  = post "DeleteRetentionPolicy"
    response = nullResponse DeleteRetentionPolicyResponse
