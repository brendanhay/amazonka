{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PasswordPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PasswordPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the account password policy.
--
--
-- This data type is used as a response element in the 'GetAccountPasswordPolicy' operation.
--
--
-- /See:/ 'passwordPolicy' smart constructor.
data PasswordPolicy = PasswordPolicy'
  { _ppExpirePasswords ::
      !(Maybe Bool),
    _ppMinimumPasswordLength :: !(Maybe Nat),
    _ppRequireNumbers :: !(Maybe Bool),
    _ppPasswordReusePrevention :: !(Maybe Nat),
    _ppRequireLowercaseCharacters :: !(Maybe Bool),
    _ppMaxPasswordAge :: !(Maybe Nat),
    _ppHardExpiry :: !(Maybe Bool),
    _ppRequireSymbols :: !(Maybe Bool),
    _ppRequireUppercaseCharacters :: !(Maybe Bool),
    _ppAllowUsersToChangePassword :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PasswordPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppExpirePasswords' - Indicates whether passwords in the account expire. Returns true if @MaxPasswordAge@ contains a value greater than 0. Returns false if MaxPasswordAge is 0 or not present.
--
-- * 'ppMinimumPasswordLength' - Minimum length to require for IAM user passwords.
--
-- * 'ppRequireNumbers' - Specifies whether to require numbers for IAM user passwords.
--
-- * 'ppPasswordReusePrevention' - Specifies the number of previous passwords that IAM users are prevented from reusing.
--
-- * 'ppRequireLowercaseCharacters' - Specifies whether to require lowercase characters for IAM user passwords.
--
-- * 'ppMaxPasswordAge' - The number of days that an IAM user password is valid.
--
-- * 'ppHardExpiry' - Specifies whether IAM users are prevented from setting a new password after their password has expired.
--
-- * 'ppRequireSymbols' - Specifies whether to require symbols for IAM user passwords.
--
-- * 'ppRequireUppercaseCharacters' - Specifies whether to require uppercase characters for IAM user passwords.
--
-- * 'ppAllowUsersToChangePassword' - Specifies whether IAM users are allowed to change their own password.
passwordPolicy ::
  PasswordPolicy
passwordPolicy =
  PasswordPolicy'
    { _ppExpirePasswords = Nothing,
      _ppMinimumPasswordLength = Nothing,
      _ppRequireNumbers = Nothing,
      _ppPasswordReusePrevention = Nothing,
      _ppRequireLowercaseCharacters = Nothing,
      _ppMaxPasswordAge = Nothing,
      _ppHardExpiry = Nothing,
      _ppRequireSymbols = Nothing,
      _ppRequireUppercaseCharacters = Nothing,
      _ppAllowUsersToChangePassword = Nothing
    }

-- | Indicates whether passwords in the account expire. Returns true if @MaxPasswordAge@ contains a value greater than 0. Returns false if MaxPasswordAge is 0 or not present.
ppExpirePasswords :: Lens' PasswordPolicy (Maybe Bool)
ppExpirePasswords = lens _ppExpirePasswords (\s a -> s {_ppExpirePasswords = a})

-- | Minimum length to require for IAM user passwords.
ppMinimumPasswordLength :: Lens' PasswordPolicy (Maybe Natural)
ppMinimumPasswordLength = lens _ppMinimumPasswordLength (\s a -> s {_ppMinimumPasswordLength = a}) . mapping _Nat

-- | Specifies whether to require numbers for IAM user passwords.
ppRequireNumbers :: Lens' PasswordPolicy (Maybe Bool)
ppRequireNumbers = lens _ppRequireNumbers (\s a -> s {_ppRequireNumbers = a})

-- | Specifies the number of previous passwords that IAM users are prevented from reusing.
ppPasswordReusePrevention :: Lens' PasswordPolicy (Maybe Natural)
ppPasswordReusePrevention = lens _ppPasswordReusePrevention (\s a -> s {_ppPasswordReusePrevention = a}) . mapping _Nat

-- | Specifies whether to require lowercase characters for IAM user passwords.
ppRequireLowercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireLowercaseCharacters = lens _ppRequireLowercaseCharacters (\s a -> s {_ppRequireLowercaseCharacters = a})

-- | The number of days that an IAM user password is valid.
ppMaxPasswordAge :: Lens' PasswordPolicy (Maybe Natural)
ppMaxPasswordAge = lens _ppMaxPasswordAge (\s a -> s {_ppMaxPasswordAge = a}) . mapping _Nat

-- | Specifies whether IAM users are prevented from setting a new password after their password has expired.
ppHardExpiry :: Lens' PasswordPolicy (Maybe Bool)
ppHardExpiry = lens _ppHardExpiry (\s a -> s {_ppHardExpiry = a})

-- | Specifies whether to require symbols for IAM user passwords.
ppRequireSymbols :: Lens' PasswordPolicy (Maybe Bool)
ppRequireSymbols = lens _ppRequireSymbols (\s a -> s {_ppRequireSymbols = a})

-- | Specifies whether to require uppercase characters for IAM user passwords.
ppRequireUppercaseCharacters :: Lens' PasswordPolicy (Maybe Bool)
ppRequireUppercaseCharacters = lens _ppRequireUppercaseCharacters (\s a -> s {_ppRequireUppercaseCharacters = a})

-- | Specifies whether IAM users are allowed to change their own password.
ppAllowUsersToChangePassword :: Lens' PasswordPolicy (Maybe Bool)
ppAllowUsersToChangePassword = lens _ppAllowUsersToChangePassword (\s a -> s {_ppAllowUsersToChangePassword = a})

instance FromXML PasswordPolicy where
  parseXML x =
    PasswordPolicy'
      <$> (x .@? "ExpirePasswords")
      <*> (x .@? "MinimumPasswordLength")
      <*> (x .@? "RequireNumbers")
      <*> (x .@? "PasswordReusePrevention")
      <*> (x .@? "RequireLowercaseCharacters")
      <*> (x .@? "MaxPasswordAge")
      <*> (x .@? "HardExpiry")
      <*> (x .@? "RequireSymbols")
      <*> (x .@? "RequireUppercaseCharacters")
      <*> (x .@? "AllowUsersToChangePassword")

instance Hashable PasswordPolicy

instance NFData PasswordPolicy
