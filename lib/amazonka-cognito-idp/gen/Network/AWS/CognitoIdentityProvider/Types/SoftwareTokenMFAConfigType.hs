{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFAConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFAConfigType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The type used for enabling software token MFA at the user pool level.
--
--
--
-- /See:/ 'softwareTokenMFAConfigType' smart constructor.
newtype SoftwareTokenMFAConfigType = SoftwareTokenMFAConfigType'
  { _stmctEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SoftwareTokenMFAConfigType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stmctEnabled' - Specifies whether software token MFA is enabled.
softwareTokenMFAConfigType ::
  SoftwareTokenMFAConfigType
softwareTokenMFAConfigType =
  SoftwareTokenMFAConfigType' {_stmctEnabled = Nothing}

-- | Specifies whether software token MFA is enabled.
stmctEnabled :: Lens' SoftwareTokenMFAConfigType (Maybe Bool)
stmctEnabled = lens _stmctEnabled (\s a -> s {_stmctEnabled = a})

instance FromJSON SoftwareTokenMFAConfigType where
  parseJSON =
    withObject
      "SoftwareTokenMFAConfigType"
      (\x -> SoftwareTokenMFAConfigType' <$> (x .:? "Enabled"))

instance Hashable SoftwareTokenMFAConfigType

instance NFData SoftwareTokenMFAConfigType

instance ToJSON SoftwareTokenMFAConfigType where
  toJSON SoftwareTokenMFAConfigType' {..} =
    object (catMaybes [("Enabled" .=) <$> _stmctEnabled])
