{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Activation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Activation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.Tag

-- | An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.
--
--
--
-- /See:/ 'activation' smart constructor.
data Activation = Activation'
  { _aExpired :: !(Maybe Bool),
    _aDefaultInstanceName :: !(Maybe Text),
    _aActivationId :: !(Maybe Text),
    _aCreatedDate :: !(Maybe POSIX),
    _aRegistrationLimit :: !(Maybe Nat),
    _aExpirationDate :: !(Maybe POSIX),
    _aDescription :: !(Maybe Text),
    _aTags :: !(Maybe [Tag]),
    _aRegistrationsCount :: !(Maybe Nat),
    _aIAMRole :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Activation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aExpired' - Whether or not the activation is expired.
--
-- * 'aDefaultInstanceName' - A name for the managed instance when it is created.
--
-- * 'aActivationId' - The ID created by Systems Manager when you submitted the activation.
--
-- * 'aCreatedDate' - The date the activation was created.
--
-- * 'aRegistrationLimit' - The maximum number of managed instances that can be registered using this activation.
--
-- * 'aExpirationDate' - The date when this activation can no longer be used to register managed instances.
--
-- * 'aDescription' - A user defined description of the activation.
--
-- * 'aTags' - Tags assigned to the activation.
--
-- * 'aRegistrationsCount' - The number of managed instances already registered with this activation.
--
-- * 'aIAMRole' - The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
activation ::
  Activation
activation =
  Activation'
    { _aExpired = Nothing,
      _aDefaultInstanceName = Nothing,
      _aActivationId = Nothing,
      _aCreatedDate = Nothing,
      _aRegistrationLimit = Nothing,
      _aExpirationDate = Nothing,
      _aDescription = Nothing,
      _aTags = Nothing,
      _aRegistrationsCount = Nothing,
      _aIAMRole = Nothing
    }

-- | Whether or not the activation is expired.
aExpired :: Lens' Activation (Maybe Bool)
aExpired = lens _aExpired (\s a -> s {_aExpired = a})

-- | A name for the managed instance when it is created.
aDefaultInstanceName :: Lens' Activation (Maybe Text)
aDefaultInstanceName = lens _aDefaultInstanceName (\s a -> s {_aDefaultInstanceName = a})

-- | The ID created by Systems Manager when you submitted the activation.
aActivationId :: Lens' Activation (Maybe Text)
aActivationId = lens _aActivationId (\s a -> s {_aActivationId = a})

-- | The date the activation was created.
aCreatedDate :: Lens' Activation (Maybe UTCTime)
aCreatedDate = lens _aCreatedDate (\s a -> s {_aCreatedDate = a}) . mapping _Time

-- | The maximum number of managed instances that can be registered using this activation.
aRegistrationLimit :: Lens' Activation (Maybe Natural)
aRegistrationLimit = lens _aRegistrationLimit (\s a -> s {_aRegistrationLimit = a}) . mapping _Nat

-- | The date when this activation can no longer be used to register managed instances.
aExpirationDate :: Lens' Activation (Maybe UTCTime)
aExpirationDate = lens _aExpirationDate (\s a -> s {_aExpirationDate = a}) . mapping _Time

-- | A user defined description of the activation.
aDescription :: Lens' Activation (Maybe Text)
aDescription = lens _aDescription (\s a -> s {_aDescription = a})

-- | Tags assigned to the activation.
aTags :: Lens' Activation [Tag]
aTags = lens _aTags (\s a -> s {_aTags = a}) . _Default . _Coerce

-- | The number of managed instances already registered with this activation.
aRegistrationsCount :: Lens' Activation (Maybe Natural)
aRegistrationsCount = lens _aRegistrationsCount (\s a -> s {_aRegistrationsCount = a}) . mapping _Nat

-- | The Amazon Identity and Access Management (IAM) role to assign to the managed instance.
aIAMRole :: Lens' Activation (Maybe Text)
aIAMRole = lens _aIAMRole (\s a -> s {_aIAMRole = a})

instance FromJSON Activation where
  parseJSON =
    withObject
      "Activation"
      ( \x ->
          Activation'
            <$> (x .:? "Expired")
            <*> (x .:? "DefaultInstanceName")
            <*> (x .:? "ActivationId")
            <*> (x .:? "CreatedDate")
            <*> (x .:? "RegistrationLimit")
            <*> (x .:? "ExpirationDate")
            <*> (x .:? "Description")
            <*> (x .:? "Tags" .!= mempty)
            <*> (x .:? "RegistrationsCount")
            <*> (x .:? "IamRole")
      )

instance Hashable Activation

instance NFData Activation
