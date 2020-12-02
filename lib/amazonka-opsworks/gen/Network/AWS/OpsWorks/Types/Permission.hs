{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Permission where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes stack or user permissions.
--
--
--
-- /See:/ 'permission' smart constructor.
data Permission = Permission'
  { _pIAMUserARN :: !(Maybe Text),
    _pAllowSudo :: !(Maybe Bool),
    _pStackId :: !(Maybe Text),
    _pLevel :: !(Maybe Text),
    _pAllowSSH :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pIAMUserARN' - The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'pAllowSudo' - Whether the user can use __sudo__ .
--
-- * 'pStackId' - A stack ID.
--
-- * 'pLevel' - The user's permission level, which must be the following:     * @deny@      * @show@      * @deploy@      * @manage@      * @iam_only@  For more information on the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
--
-- * 'pAllowSSH' - Whether the user can use SSH.
permission ::
  Permission
permission =
  Permission'
    { _pIAMUserARN = Nothing,
      _pAllowSudo = Nothing,
      _pStackId = Nothing,
      _pLevel = Nothing,
      _pAllowSSH = Nothing
    }

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
pIAMUserARN :: Lens' Permission (Maybe Text)
pIAMUserARN = lens _pIAMUserARN (\s a -> s {_pIAMUserARN = a})

-- | Whether the user can use __sudo__ .
pAllowSudo :: Lens' Permission (Maybe Bool)
pAllowSudo = lens _pAllowSudo (\s a -> s {_pAllowSudo = a})

-- | A stack ID.
pStackId :: Lens' Permission (Maybe Text)
pStackId = lens _pStackId (\s a -> s {_pStackId = a})

-- | The user's permission level, which must be the following:     * @deny@      * @show@      * @deploy@      * @manage@      * @iam_only@  For more information on the permissions associated with these levels, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
pLevel :: Lens' Permission (Maybe Text)
pLevel = lens _pLevel (\s a -> s {_pLevel = a})

-- | Whether the user can use SSH.
pAllowSSH :: Lens' Permission (Maybe Bool)
pAllowSSH = lens _pAllowSSH (\s a -> s {_pAllowSSH = a})

instance FromJSON Permission where
  parseJSON =
    withObject
      "Permission"
      ( \x ->
          Permission'
            <$> (x .:? "IamUserArn")
            <*> (x .:? "AllowSudo")
            <*> (x .:? "StackId")
            <*> (x .:? "Level")
            <*> (x .:? "AllowSsh")
      )

instance Hashable Permission

instance NFData Permission
