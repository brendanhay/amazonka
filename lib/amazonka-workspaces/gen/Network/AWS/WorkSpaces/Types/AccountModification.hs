{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.AccountModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.AccountModification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum

-- | Describes a modification to the configuration of Bring Your Own License (BYOL) for the specified account.
--
--
--
-- /See:/ 'accountModification' smart constructor.
data AccountModification = AccountModification'
  { _amStartTime ::
      !(Maybe POSIX),
    _amDedicatedTenancySupport ::
      !(Maybe DedicatedTenancySupportResultEnum),
    _amModificationState ::
      !(Maybe DedicatedTenancyModificationStateEnum),
    _amDedicatedTenancyManagementCidrRange ::
      !(Maybe Text),
    _amErrorCode :: !(Maybe Text),
    _amErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountModification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amStartTime' - The timestamp when the modification of the BYOL configuration was started.
--
-- * 'amDedicatedTenancySupport' - The status of BYOL (whether BYOL is being enabled or disabled).
--
-- * 'amModificationState' - The state of the modification to the configuration of BYOL.
--
-- * 'amDedicatedTenancyManagementCidrRange' - The IP address range, specified as an IPv4 CIDR block, for the management network interface used for the account.
--
-- * 'amErrorCode' - The error code that is returned if the configuration of BYOL cannot be modified.
--
-- * 'amErrorMessage' - The text of the error message that is returned if the configuration of BYOL cannot be modified.
accountModification ::
  AccountModification
accountModification =
  AccountModification'
    { _amStartTime = Nothing,
      _amDedicatedTenancySupport = Nothing,
      _amModificationState = Nothing,
      _amDedicatedTenancyManagementCidrRange = Nothing,
      _amErrorCode = Nothing,
      _amErrorMessage = Nothing
    }

-- | The timestamp when the modification of the BYOL configuration was started.
amStartTime :: Lens' AccountModification (Maybe UTCTime)
amStartTime = lens _amStartTime (\s a -> s {_amStartTime = a}) . mapping _Time

-- | The status of BYOL (whether BYOL is being enabled or disabled).
amDedicatedTenancySupport :: Lens' AccountModification (Maybe DedicatedTenancySupportResultEnum)
amDedicatedTenancySupport = lens _amDedicatedTenancySupport (\s a -> s {_amDedicatedTenancySupport = a})

-- | The state of the modification to the configuration of BYOL.
amModificationState :: Lens' AccountModification (Maybe DedicatedTenancyModificationStateEnum)
amModificationState = lens _amModificationState (\s a -> s {_amModificationState = a})

-- | The IP address range, specified as an IPv4 CIDR block, for the management network interface used for the account.
amDedicatedTenancyManagementCidrRange :: Lens' AccountModification (Maybe Text)
amDedicatedTenancyManagementCidrRange = lens _amDedicatedTenancyManagementCidrRange (\s a -> s {_amDedicatedTenancyManagementCidrRange = a})

-- | The error code that is returned if the configuration of BYOL cannot be modified.
amErrorCode :: Lens' AccountModification (Maybe Text)
amErrorCode = lens _amErrorCode (\s a -> s {_amErrorCode = a})

-- | The text of the error message that is returned if the configuration of BYOL cannot be modified.
amErrorMessage :: Lens' AccountModification (Maybe Text)
amErrorMessage = lens _amErrorMessage (\s a -> s {_amErrorMessage = a})

instance FromJSON AccountModification where
  parseJSON =
    withObject
      "AccountModification"
      ( \x ->
          AccountModification'
            <$> (x .:? "StartTime")
            <*> (x .:? "DedicatedTenancySupport")
            <*> (x .:? "ModificationState")
            <*> (x .:? "DedicatedTenancyManagementCidrRange")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable AccountModification

instance NFData AccountModification
