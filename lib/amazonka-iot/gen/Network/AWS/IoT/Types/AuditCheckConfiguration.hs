{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Which audit checks are enabled and disabled for this account.
--
--
--
-- /See:/ 'auditCheckConfiguration' smart constructor.
newtype AuditCheckConfiguration = AuditCheckConfiguration'
  { _accEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuditCheckConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'accEnabled' - True if this audit check is enabled for this account.
auditCheckConfiguration ::
  AuditCheckConfiguration
auditCheckConfiguration =
  AuditCheckConfiguration' {_accEnabled = Nothing}

-- | True if this audit check is enabled for this account.
accEnabled :: Lens' AuditCheckConfiguration (Maybe Bool)
accEnabled = lens _accEnabled (\s a -> s {_accEnabled = a})

instance FromJSON AuditCheckConfiguration where
  parseJSON =
    withObject
      "AuditCheckConfiguration"
      (\x -> AuditCheckConfiguration' <$> (x .:? "enabled"))

instance Hashable AuditCheckConfiguration

instance NFData AuditCheckConfiguration

instance ToJSON AuditCheckConfiguration where
  toJSON AuditCheckConfiguration' {..} =
    object (catMaybes [("enabled" .=) <$> _accEnabled])
