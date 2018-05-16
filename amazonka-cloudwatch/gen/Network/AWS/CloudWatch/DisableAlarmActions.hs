{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DisableAlarmActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the actions for the specified alarms. When an alarm's actions are disabled, the alarm actions do not execute when the alarm state changes.
--
--
module Network.AWS.CloudWatch.DisableAlarmActions
    (
    -- * Creating a Request
      disableAlarmActions
    , DisableAlarmActions
    -- * Request Lenses
    , daaAlarmNames

    -- * Destructuring the Response
    , disableAlarmActionsResponse
    , DisableAlarmActionsResponse
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableAlarmActions' smart constructor.
newtype DisableAlarmActions = DisableAlarmActions'
  { _daaAlarmNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableAlarmActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daaAlarmNames' - The names of the alarms.
disableAlarmActions
    :: DisableAlarmActions
disableAlarmActions = DisableAlarmActions' {_daaAlarmNames = mempty}


-- | The names of the alarms.
daaAlarmNames :: Lens' DisableAlarmActions [Text]
daaAlarmNames = lens _daaAlarmNames (\ s a -> s{_daaAlarmNames = a}) . _Coerce

instance AWSRequest DisableAlarmActions where
        type Rs DisableAlarmActions =
             DisableAlarmActionsResponse
        request = postQuery cloudWatch
        response = receiveNull DisableAlarmActionsResponse'

instance Hashable DisableAlarmActions where

instance NFData DisableAlarmActions where

instance ToHeaders DisableAlarmActions where
        toHeaders = const mempty

instance ToPath DisableAlarmActions where
        toPath = const "/"

instance ToQuery DisableAlarmActions where
        toQuery DisableAlarmActions'{..}
          = mconcat
              ["Action" =: ("DisableAlarmActions" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmNames" =: toQueryList "member" _daaAlarmNames]

-- | /See:/ 'disableAlarmActionsResponse' smart constructor.
data DisableAlarmActionsResponse =
  DisableAlarmActionsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableAlarmActionsResponse' with the minimum fields required to make a request.
--
disableAlarmActionsResponse
    :: DisableAlarmActionsResponse
disableAlarmActionsResponse = DisableAlarmActionsResponse'


instance NFData DisableAlarmActionsResponse where
