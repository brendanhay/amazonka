{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The password policy type.
--
--
--
-- /See:/ 'passwordPolicyType' smart constructor.
data PasswordPolicyType = PasswordPolicyType'
  { _pptRequireNumbers ::
      !(Maybe Bool),
    _pptRequireUppercase :: !(Maybe Bool),
    _pptRequireLowercase :: !(Maybe Bool),
    _pptMinimumLength :: !(Maybe Nat),
    _pptRequireSymbols :: !(Maybe Bool),
    _pptTemporaryPasswordValidityDays :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PasswordPolicyType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pptRequireNumbers' - In the password policy that you have set, refers to whether you have required users to use at least one number in their password.
--
-- * 'pptRequireUppercase' - In the password policy that you have set, refers to whether you have required users to use at least one uppercase letter in their password.
--
-- * 'pptRequireLowercase' - In the password policy that you have set, refers to whether you have required users to use at least one lowercase letter in their password.
--
-- * 'pptMinimumLength' - The minimum length of the password policy that you have set. Cannot be less than 6.
--
-- * 'pptRequireSymbols' - In the password policy that you have set, refers to whether you have required users to use at least one symbol in their password.
--
-- * 'pptTemporaryPasswordValidityDays' - In the password policy you have set, refers to the number of days a temporary password is valid. If the user does not sign-in during this time, their password will need to be reset by an administrator.
passwordPolicyType ::
  PasswordPolicyType
passwordPolicyType =
  PasswordPolicyType'
    { _pptRequireNumbers = Nothing,
      _pptRequireUppercase = Nothing,
      _pptRequireLowercase = Nothing,
      _pptMinimumLength = Nothing,
      _pptRequireSymbols = Nothing,
      _pptTemporaryPasswordValidityDays = Nothing
    }

-- | In the password policy that you have set, refers to whether you have required users to use at least one number in their password.
pptRequireNumbers :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireNumbers = lens _pptRequireNumbers (\s a -> s {_pptRequireNumbers = a})

-- | In the password policy that you have set, refers to whether you have required users to use at least one uppercase letter in their password.
pptRequireUppercase :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireUppercase = lens _pptRequireUppercase (\s a -> s {_pptRequireUppercase = a})

-- | In the password policy that you have set, refers to whether you have required users to use at least one lowercase letter in their password.
pptRequireLowercase :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireLowercase = lens _pptRequireLowercase (\s a -> s {_pptRequireLowercase = a})

-- | The minimum length of the password policy that you have set. Cannot be less than 6.
pptMinimumLength :: Lens' PasswordPolicyType (Maybe Natural)
pptMinimumLength = lens _pptMinimumLength (\s a -> s {_pptMinimumLength = a}) . mapping _Nat

-- | In the password policy that you have set, refers to whether you have required users to use at least one symbol in their password.
pptRequireSymbols :: Lens' PasswordPolicyType (Maybe Bool)
pptRequireSymbols = lens _pptRequireSymbols (\s a -> s {_pptRequireSymbols = a})

-- | In the password policy you have set, refers to the number of days a temporary password is valid. If the user does not sign-in during this time, their password will need to be reset by an administrator.
pptTemporaryPasswordValidityDays :: Lens' PasswordPolicyType (Maybe Natural)
pptTemporaryPasswordValidityDays = lens _pptTemporaryPasswordValidityDays (\s a -> s {_pptTemporaryPasswordValidityDays = a}) . mapping _Nat

instance FromJSON PasswordPolicyType where
  parseJSON =
    withObject
      "PasswordPolicyType"
      ( \x ->
          PasswordPolicyType'
            <$> (x .:? "RequireNumbers")
            <*> (x .:? "RequireUppercase")
            <*> (x .:? "RequireLowercase")
            <*> (x .:? "MinimumLength")
            <*> (x .:? "RequireSymbols")
            <*> (x .:? "TemporaryPasswordValidityDays")
      )

instance Hashable PasswordPolicyType

instance NFData PasswordPolicyType

instance ToJSON PasswordPolicyType where
  toJSON PasswordPolicyType' {..} =
    object
      ( catMaybes
          [ ("RequireNumbers" .=) <$> _pptRequireNumbers,
            ("RequireUppercase" .=) <$> _pptRequireUppercase,
            ("RequireLowercase" .=) <$> _pptRequireLowercase,
            ("MinimumLength" .=) <$> _pptMinimumLength,
            ("RequireSymbols" .=) <$> _pptRequireSymbols,
            ("TemporaryPasswordValidityDays" .=)
              <$> _pptTemporaryPasswordValidityDays
          ]
      )
