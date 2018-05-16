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
-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the actions for the specified alarms.
--
--
module Network.AWS.CloudWatch.EnableAlarmActions
    (
    -- * Creating a Request
      enableAlarmActions
    , EnableAlarmActions
    -- * Request Lenses
    , eaaAlarmNames

    -- * Destructuring the Response
    , enableAlarmActionsResponse
    , EnableAlarmActionsResponse
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableAlarmActions' smart constructor.
newtype EnableAlarmActions = EnableAlarmActions'
  { _eaaAlarmNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableAlarmActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaaAlarmNames' - The names of the alarms.
enableAlarmActions
    :: EnableAlarmActions
enableAlarmActions = EnableAlarmActions' {_eaaAlarmNames = mempty}


-- | The names of the alarms.
eaaAlarmNames :: Lens' EnableAlarmActions [Text]
eaaAlarmNames = lens _eaaAlarmNames (\ s a -> s{_eaaAlarmNames = a}) . _Coerce

instance AWSRequest EnableAlarmActions where
        type Rs EnableAlarmActions =
             EnableAlarmActionsResponse
        request = postQuery cloudWatch
        response = receiveNull EnableAlarmActionsResponse'

instance Hashable EnableAlarmActions where

instance NFData EnableAlarmActions where

instance ToHeaders EnableAlarmActions where
        toHeaders = const mempty

instance ToPath EnableAlarmActions where
        toPath = const "/"

instance ToQuery EnableAlarmActions where
        toQuery EnableAlarmActions'{..}
          = mconcat
              ["Action" =: ("EnableAlarmActions" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmNames" =: toQueryList "member" _eaaAlarmNames]

-- | /See:/ 'enableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse =
  EnableAlarmActionsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableAlarmActionsResponse' with the minimum fields required to make a request.
--
enableAlarmActionsResponse
    :: EnableAlarmActionsResponse
enableAlarmActionsResponse = EnableAlarmActionsResponse'


instance NFData EnableAlarmActionsResponse where
