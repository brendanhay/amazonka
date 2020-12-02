{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceCredentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Set of credentials required to remotely access a fleet instance. Access credentials are requested by calling 'GetInstanceAccess' and returned in an 'InstanceAccess' object.
--
--
--
-- /See:/ 'instanceCredentials' smart constructor.
data InstanceCredentials = InstanceCredentials'
  { _icUserName ::
      !(Maybe Text),
    _icSecret :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icUserName' - User login string.
--
-- * 'icSecret' - Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
instanceCredentials ::
  InstanceCredentials
instanceCredentials =
  InstanceCredentials' {_icUserName = Nothing, _icSecret = Nothing}

-- | User login string.
icUserName :: Lens' InstanceCredentials (Maybe Text)
icUserName = lens _icUserName (\s a -> s {_icUserName = a})

-- | Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
icSecret :: Lens' InstanceCredentials (Maybe Text)
icSecret = lens _icSecret (\s a -> s {_icSecret = a})

instance FromJSON InstanceCredentials where
  parseJSON =
    withObject
      "InstanceCredentials"
      ( \x ->
          InstanceCredentials' <$> (x .:? "UserName") <*> (x .:? "Secret")
      )

instance Hashable InstanceCredentials

instance NFData InstanceCredentials
