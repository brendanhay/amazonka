{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.NotifyAppValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS SMS about whether application validation is successful.
module Network.AWS.SMS.NotifyAppValidationOutput
  ( -- * Creating a Request
    notifyAppValidationOutput,
    NotifyAppValidationOutput,

    -- * Request Lenses
    navoNotificationContext,
    navoAppId,

    -- * Destructuring the Response
    notifyAppValidationOutputResponse,
    NotifyAppValidationOutputResponse,

    -- * Response Lenses
    navorsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'notifyAppValidationOutput' smart constructor.
data NotifyAppValidationOutput = NotifyAppValidationOutput'
  { _navoNotificationContext ::
      !(Maybe NotificationContext),
    _navoAppId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotifyAppValidationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'navoNotificationContext' - The notification information.
--
-- * 'navoAppId' - The ID of the application.
notifyAppValidationOutput ::
  -- | 'navoAppId'
  Text ->
  NotifyAppValidationOutput
notifyAppValidationOutput pAppId_ =
  NotifyAppValidationOutput'
    { _navoNotificationContext = Nothing,
      _navoAppId = pAppId_
    }

-- | The notification information.
navoNotificationContext :: Lens' NotifyAppValidationOutput (Maybe NotificationContext)
navoNotificationContext = lens _navoNotificationContext (\s a -> s {_navoNotificationContext = a})

-- | The ID of the application.
navoAppId :: Lens' NotifyAppValidationOutput Text
navoAppId = lens _navoAppId (\s a -> s {_navoAppId = a})

instance AWSRequest NotifyAppValidationOutput where
  type
    Rs NotifyAppValidationOutput =
      NotifyAppValidationOutputResponse
  request = postJSON sms
  response =
    receiveEmpty
      ( \s h x ->
          NotifyAppValidationOutputResponse' <$> (pure (fromEnum s))
      )

instance Hashable NotifyAppValidationOutput

instance NFData NotifyAppValidationOutput

instance ToHeaders NotifyAppValidationOutput where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.NotifyAppValidationOutput" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON NotifyAppValidationOutput where
  toJSON NotifyAppValidationOutput' {..} =
    object
      ( catMaybes
          [ ("notificationContext" .=) <$> _navoNotificationContext,
            Just ("appId" .= _navoAppId)
          ]
      )

instance ToPath NotifyAppValidationOutput where
  toPath = const "/"

instance ToQuery NotifyAppValidationOutput where
  toQuery = const mempty

-- | /See:/ 'notifyAppValidationOutputResponse' smart constructor.
newtype NotifyAppValidationOutputResponse = NotifyAppValidationOutputResponse'
  { _navorsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotifyAppValidationOutputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'navorsResponseStatus' - -- | The response status code.
notifyAppValidationOutputResponse ::
  -- | 'navorsResponseStatus'
  Int ->
  NotifyAppValidationOutputResponse
notifyAppValidationOutputResponse pResponseStatus_ =
  NotifyAppValidationOutputResponse'
    { _navorsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
navorsResponseStatus :: Lens' NotifyAppValidationOutputResponse Int
navorsResponseStatus = lens _navorsResponseStatus (\s a -> s {_navorsResponseStatus = a})

instance NFData NotifyAppValidationOutputResponse
