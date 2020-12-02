{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.NotificationContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.NotificationContext where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.ValidationStatus

-- | Contains the status of validating an application.
--
--
--
-- /See:/ 'notificationContext' smart constructor.
data NotificationContext = NotificationContext'
  { _ncStatus ::
      !(Maybe ValidationStatus),
    _ncStatusMessage :: !(Maybe Text),
    _ncValidationId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NotificationContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncStatus' - The status of the validation.
--
-- * 'ncStatusMessage' - The status message.
--
-- * 'ncValidationId' - The ID of the validation.
notificationContext ::
  NotificationContext
notificationContext =
  NotificationContext'
    { _ncStatus = Nothing,
      _ncStatusMessage = Nothing,
      _ncValidationId = Nothing
    }

-- | The status of the validation.
ncStatus :: Lens' NotificationContext (Maybe ValidationStatus)
ncStatus = lens _ncStatus (\s a -> s {_ncStatus = a})

-- | The status message.
ncStatusMessage :: Lens' NotificationContext (Maybe Text)
ncStatusMessage = lens _ncStatusMessage (\s a -> s {_ncStatusMessage = a})

-- | The ID of the validation.
ncValidationId :: Lens' NotificationContext (Maybe Text)
ncValidationId = lens _ncValidationId (\s a -> s {_ncValidationId = a})

instance Hashable NotificationContext

instance NFData NotificationContext

instance ToJSON NotificationContext where
  toJSON NotificationContext' {..} =
    object
      ( catMaybes
          [ ("status" .=) <$> _ncStatus,
            ("statusMessage" .=) <$> _ncStatusMessage,
            ("validationId" .=) <$> _ncValidationId
          ]
      )
