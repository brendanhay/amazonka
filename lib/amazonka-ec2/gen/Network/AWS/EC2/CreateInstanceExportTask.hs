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
-- Module      : Network.AWS.EC2.CreateInstanceExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a running or stopped instance to an Amazon S3 bucket.
--
--
-- For information about the supported operating systems, image formats, and known limitations for the types of instances you can export, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html Exporting an Instance as a VM Using VM Import/Export> in the /VM Import\/Export User Guide/ .
module Network.AWS.EC2.CreateInstanceExportTask
  ( -- * Creating a Request
    createInstanceExportTask,
    CreateInstanceExportTask,

    -- * Request Lenses
    cietTagSpecifications,
    cietDescription,
    cietExportToS3Task,
    cietInstanceId,
    cietTargetEnvironment,

    -- * Destructuring the Response
    createInstanceExportTaskResponse,
    CreateInstanceExportTaskResponse,

    -- * Response Lenses
    cietrsExportTask,
    cietrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createInstanceExportTask' smart constructor.
data CreateInstanceExportTask = CreateInstanceExportTask'
  { _cietTagSpecifications ::
      !(Maybe [TagSpecification]),
    _cietDescription :: !(Maybe Text),
    _cietExportToS3Task ::
      !ExportToS3TaskSpecification,
    _cietInstanceId :: !Text,
    _cietTargetEnvironment ::
      !ExportEnvironment
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInstanceExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cietTagSpecifications' - The tags to apply to the instance export task during creation.
--
-- * 'cietDescription' - A description for the conversion task or the resource being exported. The maximum length is 255 characters.
--
-- * 'cietExportToS3Task' - The format and location for an instance export task.
--
-- * 'cietInstanceId' - The ID of the instance.
--
-- * 'cietTargetEnvironment' - The target virtualization environment.
createInstanceExportTask ::
  -- | 'cietExportToS3Task'
  ExportToS3TaskSpecification ->
  -- | 'cietInstanceId'
  Text ->
  -- | 'cietTargetEnvironment'
  ExportEnvironment ->
  CreateInstanceExportTask
createInstanceExportTask
  pExportToS3Task_
  pInstanceId_
  pTargetEnvironment_ =
    CreateInstanceExportTask'
      { _cietTagSpecifications = Nothing,
        _cietDescription = Nothing,
        _cietExportToS3Task = pExportToS3Task_,
        _cietInstanceId = pInstanceId_,
        _cietTargetEnvironment = pTargetEnvironment_
      }

-- | The tags to apply to the instance export task during creation.
cietTagSpecifications :: Lens' CreateInstanceExportTask [TagSpecification]
cietTagSpecifications = lens _cietTagSpecifications (\s a -> s {_cietTagSpecifications = a}) . _Default . _Coerce

-- | A description for the conversion task or the resource being exported. The maximum length is 255 characters.
cietDescription :: Lens' CreateInstanceExportTask (Maybe Text)
cietDescription = lens _cietDescription (\s a -> s {_cietDescription = a})

-- | The format and location for an instance export task.
cietExportToS3Task :: Lens' CreateInstanceExportTask ExportToS3TaskSpecification
cietExportToS3Task = lens _cietExportToS3Task (\s a -> s {_cietExportToS3Task = a})

-- | The ID of the instance.
cietInstanceId :: Lens' CreateInstanceExportTask Text
cietInstanceId = lens _cietInstanceId (\s a -> s {_cietInstanceId = a})

-- | The target virtualization environment.
cietTargetEnvironment :: Lens' CreateInstanceExportTask ExportEnvironment
cietTargetEnvironment = lens _cietTargetEnvironment (\s a -> s {_cietTargetEnvironment = a})

instance AWSRequest CreateInstanceExportTask where
  type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateInstanceExportTaskResponse'
            <$> (x .@? "exportTask") <*> (pure (fromEnum s))
      )

instance Hashable CreateInstanceExportTask

instance NFData CreateInstanceExportTask

instance ToHeaders CreateInstanceExportTask where
  toHeaders = const mempty

instance ToPath CreateInstanceExportTask where
  toPath = const "/"

instance ToQuery CreateInstanceExportTask where
  toQuery CreateInstanceExportTask' {..} =
    mconcat
      [ "Action" =: ("CreateInstanceExportTask" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          (toQueryList "TagSpecification" <$> _cietTagSpecifications),
        "Description" =: _cietDescription,
        "ExportToS3" =: _cietExportToS3Task,
        "InstanceId" =: _cietInstanceId,
        "TargetEnvironment" =: _cietTargetEnvironment
      ]

-- | /See:/ 'createInstanceExportTaskResponse' smart constructor.
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
  { _cietrsExportTask ::
      !(Maybe ExportTask),
    _cietrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateInstanceExportTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cietrsExportTask' - Information about the instance export task.
--
-- * 'cietrsResponseStatus' - -- | The response status code.
createInstanceExportTaskResponse ::
  -- | 'cietrsResponseStatus'
  Int ->
  CreateInstanceExportTaskResponse
createInstanceExportTaskResponse pResponseStatus_ =
  CreateInstanceExportTaskResponse'
    { _cietrsExportTask = Nothing,
      _cietrsResponseStatus = pResponseStatus_
    }

-- | Information about the instance export task.
cietrsExportTask :: Lens' CreateInstanceExportTaskResponse (Maybe ExportTask)
cietrsExportTask = lens _cietrsExportTask (\s a -> s {_cietrsExportTask = a})

-- | -- | The response status code.
cietrsResponseStatus :: Lens' CreateInstanceExportTaskResponse Int
cietrsResponseStatus = lens _cietrsResponseStatus (\s a -> s {_cietrsResponseStatus = a})

instance NFData CreateInstanceExportTaskResponse
