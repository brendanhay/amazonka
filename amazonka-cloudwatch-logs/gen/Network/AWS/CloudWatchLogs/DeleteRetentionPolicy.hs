{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types

newtype DeleteRetentionPolicy = DeleteRetentionPolicy
    { _drpLogGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

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

instance ToPath DeleteRetentionPolicy where
    toPath = const "/"

instance ToQuery DeleteRetentionPolicy where
    toQuery = const mempty

instance ToHeaders DeleteRetentionPolicy

instance ToBody DeleteRetentionPolicy where
    toBody = toBody . encode . _drpLogGroupName

data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteRetentionPolicyResponse' constructor.
deleteRetentionPolicyResponse :: DeleteRetentionPolicyResponse
deleteRetentionPolicyResponse = DeleteRetentionPolicyResponse

-- FromJSON

instance AWSRequest DeleteRetentionPolicy where
    type Sv DeleteRetentionPolicy = CloudWatchLogs
    type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse

    request  = post'
    response = nullaryResponse DeleteRetentionPolicyResponse
