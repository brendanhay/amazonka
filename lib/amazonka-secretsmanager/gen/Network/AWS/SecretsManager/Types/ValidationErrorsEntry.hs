{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.ValidationErrorsEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.ValidationErrorsEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Displays errors that occurred during validation of the resource policy.
--
--
--
-- /See:/ 'validationErrorsEntry' smart constructor.
data ValidationErrorsEntry = ValidationErrorsEntry'
  { _veeCheckName ::
      !(Maybe Text),
    _veeErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidationErrorsEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veeCheckName' - Checks the name of the policy.
--
-- * 'veeErrorMessage' - Displays error messages if validation encounters problems during validation of the resource policy.
validationErrorsEntry ::
  ValidationErrorsEntry
validationErrorsEntry =
  ValidationErrorsEntry'
    { _veeCheckName = Nothing,
      _veeErrorMessage = Nothing
    }

-- | Checks the name of the policy.
veeCheckName :: Lens' ValidationErrorsEntry (Maybe Text)
veeCheckName = lens _veeCheckName (\s a -> s {_veeCheckName = a})

-- | Displays error messages if validation encounters problems during validation of the resource policy.
veeErrorMessage :: Lens' ValidationErrorsEntry (Maybe Text)
veeErrorMessage = lens _veeErrorMessage (\s a -> s {_veeErrorMessage = a})

instance FromJSON ValidationErrorsEntry where
  parseJSON =
    withObject
      "ValidationErrorsEntry"
      ( \x ->
          ValidationErrorsEntry'
            <$> (x .:? "CheckName") <*> (x .:? "ErrorMessage")
      )

instance Hashable ValidationErrorsEntry

instance NFData ValidationErrorsEntry
