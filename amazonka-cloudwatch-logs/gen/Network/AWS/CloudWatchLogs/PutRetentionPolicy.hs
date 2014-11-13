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
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types

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

instance ToPath PutRetentionPolicy where
    toPath = const "/"

instance ToQuery PutRetentionPolicy where
    toQuery = const mempty

instance ToHeaders PutRetentionPolicy

instance ToBody PutRetentionPolicy where
    toBody = toBody . encode . _prpLogGroupName

data PutRetentionPolicyResponse = PutRetentionPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutRetentionPolicyResponse' constructor.
putRetentionPolicyResponse :: PutRetentionPolicyResponse
putRetentionPolicyResponse = PutRetentionPolicyResponse

-- FromJSON

instance AWSRequest PutRetentionPolicy where
    type Sv PutRetentionPolicy = CloudWatchLogs
    type Rs PutRetentionPolicy = PutRetentionPolicyResponse

    request  = post'
    response = nullaryResponse PutRetentionPolicyResponse
