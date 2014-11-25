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

-- Module      : Network.AWS.CloudWatch.DeleteAlarms
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes all specified alarms. In the event of an error, no alarms are
-- deleted.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DeleteAlarms.html>
module Network.AWS.CloudWatch.DeleteAlarms
    (
    -- * Request
      DeleteAlarms
    -- ** Request constructor
    , deleteAlarms
    -- ** Request lenses
    , da1AlarmNames

    -- * Response
    , DeleteAlarmsResponse
    -- ** Response constructor
    , deleteAlarmsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

newtype DeleteAlarms = DeleteAlarms
    { _da1AlarmNames :: List "AlarmNames" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DeleteAlarms where
    type Item DeleteAlarms = Text

    fromList = DeleteAlarms . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _da1AlarmNames

-- | 'DeleteAlarms' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'da1AlarmNames' @::@ ['Text']
--
deleteAlarms :: DeleteAlarms
deleteAlarms = DeleteAlarms
    { _da1AlarmNames = mempty
    }

-- | A list of alarms to be deleted.
--
da1AlarmNames :: Lens' DeleteAlarms [Text]
da1AlarmNames = lens _da1AlarmNames (\s a -> s { _da1AlarmNames = a }) . _List

data DeleteAlarmsResponse = DeleteAlarmsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAlarmsResponse' constructor.
deleteAlarmsResponse :: DeleteAlarmsResponse
deleteAlarmsResponse = DeleteAlarmsResponse

instance ToPath DeleteAlarms where
    toPath = const "/"

instance ToQuery DeleteAlarms where
    toQuery DeleteAlarms{..} = mconcat
        [ "AlarmNames" =? _da1AlarmNames
        ]

instance ToHeaders DeleteAlarms

instance AWSRequest DeleteAlarms where
    type Sv DeleteAlarms = CloudWatch
    type Rs DeleteAlarms = DeleteAlarmsResponse

    request  = post "DeleteAlarms"
    response = nullResponse DeleteAlarmsResponse
