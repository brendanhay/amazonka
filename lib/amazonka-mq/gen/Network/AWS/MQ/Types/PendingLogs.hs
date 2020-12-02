{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.PendingLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.PendingLogs where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The list of information about logs to be enabled for the specified broker.
--
-- /See:/ 'pendingLogs' smart constructor.
data PendingLogs = PendingLogs'
  { _plAudit :: !(Maybe Bool),
    _plGeneral :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PendingLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plAudit' - Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
--
-- * 'plGeneral' - Enables general logging.
pendingLogs ::
  PendingLogs
pendingLogs =
  PendingLogs' {_plAudit = Nothing, _plGeneral = Nothing}

-- | Enables audit logging. Every user management action made using JMX or the ActiveMQ Web Console is logged.
plAudit :: Lens' PendingLogs (Maybe Bool)
plAudit = lens _plAudit (\s a -> s {_plAudit = a})

-- | Enables general logging.
plGeneral :: Lens' PendingLogs (Maybe Bool)
plGeneral = lens _plGeneral (\s a -> s {_plGeneral = a})

instance FromJSON PendingLogs where
  parseJSON =
    withObject
      "PendingLogs"
      (\x -> PendingLogs' <$> (x .:? "audit") <*> (x .:? "general"))

instance Hashable PendingLogs

instance NFData PendingLogs
