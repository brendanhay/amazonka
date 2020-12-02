{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.ChapInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.ChapInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes Challenge-Handshake Authentication Protocol (CHAP) information that supports authentication between your gateway and iSCSI initiators.
--
--
--
-- /See:/ 'chapInfo' smart constructor.
data ChapInfo = ChapInfo'
  { _ciTargetARN :: !(Maybe Text),
    _ciSecretToAuthenticateInitiator :: !(Maybe (Sensitive Text)),
    _ciInitiatorName :: !(Maybe Text),
    _ciSecretToAuthenticateTarget :: !(Maybe (Sensitive Text))
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChapInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciTargetARN' - The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
--
-- * 'ciSecretToAuthenticateInitiator' - The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
--
-- * 'ciInitiatorName' - The iSCSI initiator that connects to the target.
--
-- * 'ciSecretToAuthenticateTarget' - The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g., Windows client).
chapInfo ::
  ChapInfo
chapInfo =
  ChapInfo'
    { _ciTargetARN = Nothing,
      _ciSecretToAuthenticateInitiator = Nothing,
      _ciInitiatorName = Nothing,
      _ciSecretToAuthenticateTarget = Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume. Valid Values: 50 to 500 lowercase letters, numbers, periods (.), and hyphens (-).
ciTargetARN :: Lens' ChapInfo (Maybe Text)
ciTargetARN = lens _ciTargetARN (\s a -> s {_ciTargetARN = a})

-- | The secret key that the initiator (for example, the Windows client) must provide to participate in mutual CHAP with the target.
ciSecretToAuthenticateInitiator :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateInitiator = lens _ciSecretToAuthenticateInitiator (\s a -> s {_ciSecretToAuthenticateInitiator = a}) . mapping _Sensitive

-- | The iSCSI initiator that connects to the target.
ciInitiatorName :: Lens' ChapInfo (Maybe Text)
ciInitiatorName = lens _ciInitiatorName (\s a -> s {_ciInitiatorName = a})

-- | The secret key that the target must provide to participate in mutual CHAP with the initiator (e.g., Windows client).
ciSecretToAuthenticateTarget :: Lens' ChapInfo (Maybe Text)
ciSecretToAuthenticateTarget = lens _ciSecretToAuthenticateTarget (\s a -> s {_ciSecretToAuthenticateTarget = a}) . mapping _Sensitive

instance FromJSON ChapInfo where
  parseJSON =
    withObject
      "ChapInfo"
      ( \x ->
          ChapInfo'
            <$> (x .:? "TargetARN")
            <*> (x .:? "SecretToAuthenticateInitiator")
            <*> (x .:? "InitiatorName")
            <*> (x .:? "SecretToAuthenticateTarget")
      )

instance Hashable ChapInfo

instance NFData ChapInfo
