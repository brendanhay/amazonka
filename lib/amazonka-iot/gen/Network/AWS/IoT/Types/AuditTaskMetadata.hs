{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditTaskMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskMetadata where

import Network.AWS.IoT.Types.AuditTaskStatus
import Network.AWS.IoT.Types.AuditTaskType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The audits that were performed.
--
--
--
-- /See:/ 'auditTaskMetadata' smart constructor.
data AuditTaskMetadata = AuditTaskMetadata'
  { _atmTaskType ::
      !(Maybe AuditTaskType),
    _atmTaskId :: !(Maybe Text),
    _atmTaskStatus :: !(Maybe AuditTaskStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuditTaskMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atmTaskType' - The type of this audit. One of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
--
-- * 'atmTaskId' - The ID of this audit.
--
-- * 'atmTaskStatus' - The status of this audit. One of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
auditTaskMetadata ::
  AuditTaskMetadata
auditTaskMetadata =
  AuditTaskMetadata'
    { _atmTaskType = Nothing,
      _atmTaskId = Nothing,
      _atmTaskStatus = Nothing
    }

-- | The type of this audit. One of "ON_DEMAND_AUDIT_TASK" or "SCHEDULED_AUDIT_TASK".
atmTaskType :: Lens' AuditTaskMetadata (Maybe AuditTaskType)
atmTaskType = lens _atmTaskType (\s a -> s {_atmTaskType = a})

-- | The ID of this audit.
atmTaskId :: Lens' AuditTaskMetadata (Maybe Text)
atmTaskId = lens _atmTaskId (\s a -> s {_atmTaskId = a})

-- | The status of this audit. One of "IN_PROGRESS", "COMPLETED", "FAILED", or "CANCELED".
atmTaskStatus :: Lens' AuditTaskMetadata (Maybe AuditTaskStatus)
atmTaskStatus = lens _atmTaskStatus (\s a -> s {_atmTaskStatus = a})

instance FromJSON AuditTaskMetadata where
  parseJSON =
    withObject
      "AuditTaskMetadata"
      ( \x ->
          AuditTaskMetadata'
            <$> (x .:? "taskType") <*> (x .:? "taskId") <*> (x .:? "taskStatus")
      )

instance Hashable AuditTaskMetadata

instance NFData AuditTaskMetadata
