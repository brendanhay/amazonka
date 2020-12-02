{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.MigrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.MigrationTask where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types.ResourceAttribute
import Network.AWS.MigrationHub.Types.Task
import Network.AWS.Prelude

-- | Represents a migration task in a migration tool.
--
--
--
-- /See:/ 'migrationTask' smart constructor.
data MigrationTask = MigrationTask'
  { _mtUpdateDateTime ::
      !(Maybe POSIX),
    _mtResourceAttributeList :: !(Maybe [ResourceAttribute]),
    _mtTask :: !(Maybe Task),
    _mtProgressUpdateStream :: !(Maybe Text),
    _mtMigrationTaskName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MigrationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtUpdateDateTime' - The timestamp when the task was gathered.
--
-- * 'mtResourceAttributeList' - Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
--
-- * 'mtTask' - Task object encapsulating task information.
--
-- * 'mtProgressUpdateStream' - A name that identifies the vendor of the migration tool being used.
--
-- * 'mtMigrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
migrationTask ::
  MigrationTask
migrationTask =
  MigrationTask'
    { _mtUpdateDateTime = Nothing,
      _mtResourceAttributeList = Nothing,
      _mtTask = Nothing,
      _mtProgressUpdateStream = Nothing,
      _mtMigrationTaskName = Nothing
    }

-- | The timestamp when the task was gathered.
mtUpdateDateTime :: Lens' MigrationTask (Maybe UTCTime)
mtUpdateDateTime = lens _mtUpdateDateTime (\s a -> s {_mtUpdateDateTime = a}) . mapping _Time

-- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
mtResourceAttributeList :: Lens' MigrationTask [ResourceAttribute]
mtResourceAttributeList = lens _mtResourceAttributeList (\s a -> s {_mtResourceAttributeList = a}) . _Default . _Coerce

-- | Task object encapsulating task information.
mtTask :: Lens' MigrationTask (Maybe Task)
mtTask = lens _mtTask (\s a -> s {_mtTask = a})

-- | A name that identifies the vendor of the migration tool being used.
mtProgressUpdateStream :: Lens' MigrationTask (Maybe Text)
mtProgressUpdateStream = lens _mtProgressUpdateStream (\s a -> s {_mtProgressUpdateStream = a})

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
mtMigrationTaskName :: Lens' MigrationTask (Maybe Text)
mtMigrationTaskName = lens _mtMigrationTaskName (\s a -> s {_mtMigrationTaskName = a})

instance FromJSON MigrationTask where
  parseJSON =
    withObject
      "MigrationTask"
      ( \x ->
          MigrationTask'
            <$> (x .:? "UpdateDateTime")
            <*> (x .:? "ResourceAttributeList" .!= mempty)
            <*> (x .:? "Task")
            <*> (x .:? "ProgressUpdateStream")
            <*> (x .:? "MigrationTaskName")
      )

instance Hashable MigrationTask

instance NFData MigrationTask
