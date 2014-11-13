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

-- Module      : Network.AWS.CloudWatchLogs.DeleteLogGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the log group with the specified name and permanently deletes all
-- the archived log events associated with it.
module Network.AWS.CloudWatchLogs.DeleteLogGroup
    (
    -- * Request
      DeleteLogGroup
    -- ** Request constructor
    , deleteLogGroup
    -- ** Request lenses
    , dlgLogGroupName

    -- * Response
    , DeleteLogGroupResponse
    -- ** Response constructor
    , deleteLogGroupResponse
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types

newtype DeleteLogGroup = DeleteLogGroup
    { _dlgLogGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteLogGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlgLogGroupName' @::@ 'Text'
--
deleteLogGroup :: Text -- ^ 'dlgLogGroupName'
               -> DeleteLogGroup
deleteLogGroup p1 = DeleteLogGroup
    { _dlgLogGroupName = p1
    }

dlgLogGroupName :: Lens' DeleteLogGroup Text
dlgLogGroupName = lens _dlgLogGroupName (\s a -> s { _dlgLogGroupName = a })

instance ToPath DeleteLogGroup where
    toPath = const "/"

instance ToQuery DeleteLogGroup where
    toQuery = const mempty

instance ToHeaders DeleteLogGroup

instance ToBody DeleteLogGroup where
    toBody = toBody . encode . _dlgLogGroupName

data DeleteLogGroupResponse = DeleteLogGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteLogGroupResponse' constructor.
deleteLogGroupResponse :: DeleteLogGroupResponse
deleteLogGroupResponse = DeleteLogGroupResponse

-- FromJSON

instance AWSRequest DeleteLogGroup where
    type Sv DeleteLogGroup = CloudWatchLogs
    type Rs DeleteLogGroup = DeleteLogGroupResponse

    request  = post'
    response = nullaryResponse DeleteLogGroupResponse
