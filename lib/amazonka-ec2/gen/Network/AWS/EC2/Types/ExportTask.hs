{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTask where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ExportTaskState
import Network.AWS.EC2.Types.ExportToS3Task
import Network.AWS.EC2.Types.InstanceExportDetails
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instance export task.
--
--
--
-- /See:/ 'exportTask' smart constructor.
data ExportTask = ExportTask'
  { _etTags :: !(Maybe [Tag]),
    _etDescription :: !Text,
    _etExportTaskId :: !Text,
    _etExportToS3Task :: !ExportToS3Task,
    _etInstanceExportDetails :: !InstanceExportDetails,
    _etState :: !ExportTaskState,
    _etStatusMessage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etTags' - The tags for the export task.
--
-- * 'etDescription' - A description of the resource being exported.
--
-- * 'etExportTaskId' - The ID of the export task.
--
-- * 'etExportToS3Task' - Information about the export task.
--
-- * 'etInstanceExportDetails' - Information about the instance to export.
--
-- * 'etState' - The state of the export task.
--
-- * 'etStatusMessage' - The status message related to the export task.
exportTask ::
  -- | 'etDescription'
  Text ->
  -- | 'etExportTaskId'
  Text ->
  -- | 'etExportToS3Task'
  ExportToS3Task ->
  -- | 'etInstanceExportDetails'
  InstanceExportDetails ->
  -- | 'etState'
  ExportTaskState ->
  -- | 'etStatusMessage'
  Text ->
  ExportTask
exportTask
  pDescription_
  pExportTaskId_
  pExportToS3Task_
  pInstanceExportDetails_
  pState_
  pStatusMessage_ =
    ExportTask'
      { _etTags = Nothing,
        _etDescription = pDescription_,
        _etExportTaskId = pExportTaskId_,
        _etExportToS3Task = pExportToS3Task_,
        _etInstanceExportDetails = pInstanceExportDetails_,
        _etState = pState_,
        _etStatusMessage = pStatusMessage_
      }

-- | The tags for the export task.
etTags :: Lens' ExportTask [Tag]
etTags = lens _etTags (\s a -> s {_etTags = a}) . _Default . _Coerce

-- | A description of the resource being exported.
etDescription :: Lens' ExportTask Text
etDescription = lens _etDescription (\s a -> s {_etDescription = a})

-- | The ID of the export task.
etExportTaskId :: Lens' ExportTask Text
etExportTaskId = lens _etExportTaskId (\s a -> s {_etExportTaskId = a})

-- | Information about the export task.
etExportToS3Task :: Lens' ExportTask ExportToS3Task
etExportToS3Task = lens _etExportToS3Task (\s a -> s {_etExportToS3Task = a})

-- | Information about the instance to export.
etInstanceExportDetails :: Lens' ExportTask InstanceExportDetails
etInstanceExportDetails = lens _etInstanceExportDetails (\s a -> s {_etInstanceExportDetails = a})

-- | The state of the export task.
etState :: Lens' ExportTask ExportTaskState
etState = lens _etState (\s a -> s {_etState = a})

-- | The status message related to the export task.
etStatusMessage :: Lens' ExportTask Text
etStatusMessage = lens _etStatusMessage (\s a -> s {_etStatusMessage = a})

instance FromXML ExportTask where
  parseXML x =
    ExportTask'
      <$> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@ "description")
      <*> (x .@ "exportTaskId")
      <*> (x .@ "exportToS3")
      <*> (x .@ "instanceExport")
      <*> (x .@ "state")
      <*> (x .@ "statusMessage")

instance Hashable ExportTask

instance NFData ExportTask
