{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CancelExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an export task in progress that is exporting a snapshot to Amazon S3. Any data that has already been written to the S3 bucket isn't removed.
module Network.AWS.RDS.CancelExportTask
  ( -- * Creating a Request
    cancelExportTask,
    CancelExportTask,

    -- * Request Lenses
    cetExportTaskIdentifier,

    -- * Destructuring the Response
    exportTask,
    ExportTask,

    -- * Response Lenses
    etTotalExtractedDataInGB,
    etStatus,
    etIAMRoleARN,
    etSourceARN,
    etExportOnly,
    etTaskStartTime,
    etWarningMessage,
    etSnapshotTime,
    etKMSKeyId,
    etTaskEndTime,
    etExportTaskIdentifier,
    etS3Prefix,
    etPercentProgress,
    etS3Bucket,
    etFailureCause,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask'
  { _cetExportTaskIdentifier ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CancelExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetExportTaskIdentifier' - The identifier of the snapshot export task to cancel.
cancelExportTask ::
  -- | 'cetExportTaskIdentifier'
  Text ->
  CancelExportTask
cancelExportTask pExportTaskIdentifier_ =
  CancelExportTask'
    { _cetExportTaskIdentifier =
        pExportTaskIdentifier_
    }

-- | The identifier of the snapshot export task to cancel.
cetExportTaskIdentifier :: Lens' CancelExportTask Text
cetExportTaskIdentifier = lens _cetExportTaskIdentifier (\s a -> s {_cetExportTaskIdentifier = a})

instance AWSRequest CancelExportTask where
  type Rs CancelExportTask = ExportTask
  request = postQuery rds
  response =
    receiveXMLWrapper
      "CancelExportTaskResult"
      (\s h x -> parseXML x)

instance Hashable CancelExportTask

instance NFData CancelExportTask

instance ToHeaders CancelExportTask where
  toHeaders = const mempty

instance ToPath CancelExportTask where
  toPath = const "/"

instance ToQuery CancelExportTask where
  toQuery CancelExportTask' {..} =
    mconcat
      [ "Action" =: ("CancelExportTask" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "ExportTaskIdentifier" =: _cetExportTaskIdentifier
      ]
