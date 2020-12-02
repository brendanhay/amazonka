{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportSnapshotTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportSnapshotTask where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SnapshotTaskDetail
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an import snapshot task.
--
--
--
-- /See:/ 'importSnapshotTask' smart constructor.
data ImportSnapshotTask = ImportSnapshotTask'
  { _istSnapshotTaskDetail ::
      !(Maybe SnapshotTaskDetail),
    _istImportTaskId :: !(Maybe Text),
    _istDescription :: !(Maybe Text),
    _istTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportSnapshotTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'istSnapshotTaskDetail' - Describes an import snapshot task.
--
-- * 'istImportTaskId' - The ID of the import snapshot task.
--
-- * 'istDescription' - A description of the import snapshot task.
--
-- * 'istTags' - The tags for the import snapshot task.
importSnapshotTask ::
  ImportSnapshotTask
importSnapshotTask =
  ImportSnapshotTask'
    { _istSnapshotTaskDetail = Nothing,
      _istImportTaskId = Nothing,
      _istDescription = Nothing,
      _istTags = Nothing
    }

-- | Describes an import snapshot task.
istSnapshotTaskDetail :: Lens' ImportSnapshotTask (Maybe SnapshotTaskDetail)
istSnapshotTaskDetail = lens _istSnapshotTaskDetail (\s a -> s {_istSnapshotTaskDetail = a})

-- | The ID of the import snapshot task.
istImportTaskId :: Lens' ImportSnapshotTask (Maybe Text)
istImportTaskId = lens _istImportTaskId (\s a -> s {_istImportTaskId = a})

-- | A description of the import snapshot task.
istDescription :: Lens' ImportSnapshotTask (Maybe Text)
istDescription = lens _istDescription (\s a -> s {_istDescription = a})

-- | The tags for the import snapshot task.
istTags :: Lens' ImportSnapshotTask [Tag]
istTags = lens _istTags (\s a -> s {_istTags = a}) . _Default . _Coerce

instance FromXML ImportSnapshotTask where
  parseXML x =
    ImportSnapshotTask'
      <$> (x .@? "snapshotTaskDetail")
      <*> (x .@? "importTaskId")
      <*> (x .@? "description")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ImportSnapshotTask

instance NFData ImportSnapshotTask
