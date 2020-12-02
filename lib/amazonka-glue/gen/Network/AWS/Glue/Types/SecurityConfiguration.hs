{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SecurityConfiguration where

import Network.AWS.Glue.Types.EncryptionConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a security configuration.
--
--
--
-- /See:/ 'securityConfiguration' smart constructor.
data SecurityConfiguration = SecurityConfiguration'
  { _secName ::
      !(Maybe Text),
    _secEncryptionConfiguration ::
      !(Maybe EncryptionConfiguration),
    _secCreatedTimeStamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'secName' - The name of the security configuration.
--
-- * 'secEncryptionConfiguration' - The encryption configuration associated with this security configuration.
--
-- * 'secCreatedTimeStamp' - The time at which this security configuration was created.
securityConfiguration ::
  SecurityConfiguration
securityConfiguration =
  SecurityConfiguration'
    { _secName = Nothing,
      _secEncryptionConfiguration = Nothing,
      _secCreatedTimeStamp = Nothing
    }

-- | The name of the security configuration.
secName :: Lens' SecurityConfiguration (Maybe Text)
secName = lens _secName (\s a -> s {_secName = a})

-- | The encryption configuration associated with this security configuration.
secEncryptionConfiguration :: Lens' SecurityConfiguration (Maybe EncryptionConfiguration)
secEncryptionConfiguration = lens _secEncryptionConfiguration (\s a -> s {_secEncryptionConfiguration = a})

-- | The time at which this security configuration was created.
secCreatedTimeStamp :: Lens' SecurityConfiguration (Maybe UTCTime)
secCreatedTimeStamp = lens _secCreatedTimeStamp (\s a -> s {_secCreatedTimeStamp = a}) . mapping _Time

instance FromJSON SecurityConfiguration where
  parseJSON =
    withObject
      "SecurityConfiguration"
      ( \x ->
          SecurityConfiguration'
            <$> (x .:? "Name")
            <*> (x .:? "EncryptionConfiguration")
            <*> (x .:? "CreatedTimeStamp")
      )

instance Hashable SecurityConfiguration

instance NFData SecurityConfiguration
