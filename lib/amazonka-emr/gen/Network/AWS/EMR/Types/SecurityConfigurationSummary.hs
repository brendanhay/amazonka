{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SecurityConfigurationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SecurityConfigurationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The creation date and time, and name, of a security configuration.
--
--
--
-- /See:/ 'securityConfigurationSummary' smart constructor.
data SecurityConfigurationSummary = SecurityConfigurationSummary'
  { _scsName ::
      !(Maybe Text),
    _scsCreationDateTime ::
      !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityConfigurationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsName' - The name of the security configuration.
--
-- * 'scsCreationDateTime' - The date and time the security configuration was created.
securityConfigurationSummary ::
  SecurityConfigurationSummary
securityConfigurationSummary =
  SecurityConfigurationSummary'
    { _scsName = Nothing,
      _scsCreationDateTime = Nothing
    }

-- | The name of the security configuration.
scsName :: Lens' SecurityConfigurationSummary (Maybe Text)
scsName = lens _scsName (\s a -> s {_scsName = a})

-- | The date and time the security configuration was created.
scsCreationDateTime :: Lens' SecurityConfigurationSummary (Maybe UTCTime)
scsCreationDateTime = lens _scsCreationDateTime (\s a -> s {_scsCreationDateTime = a}) . mapping _Time

instance FromJSON SecurityConfigurationSummary where
  parseJSON =
    withObject
      "SecurityConfigurationSummary"
      ( \x ->
          SecurityConfigurationSummary'
            <$> (x .:? "Name") <*> (x .:? "CreationDateTime")
      )

instance Hashable SecurityConfigurationSummary

instance NFData SecurityConfigurationSummary
