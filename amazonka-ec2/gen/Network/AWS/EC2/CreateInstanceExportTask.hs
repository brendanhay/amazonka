{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateInstanceExportTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a running or stopped instance to an S3 bucket.
--
--
-- For information about the supported operating systems, image formats, and known limitations for the types of instances you can export, see <http://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html Exporting an Instance as a VM Using VM Import/Export> in the /VM Import\/Export User Guide/ .
--
module Network.AWS.EC2.CreateInstanceExportTask
    (
    -- * Creating a Request
      createInstanceExportTask
    , CreateInstanceExportTask
    -- * Request Lenses
    , cietTargetEnvironment
    , cietExportToS3Task
    , cietDescription
    , cietInstanceId

    -- * Destructuring the Response
    , createInstanceExportTaskResponse
    , CreateInstanceExportTaskResponse
    -- * Response Lenses
    , cietrsExportTask
    , cietrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateInstanceExportTask.
--
--
--
-- /See:/ 'createInstanceExportTask' smart constructor.
data CreateInstanceExportTask = CreateInstanceExportTask'
  { _cietTargetEnvironment :: !(Maybe ExportEnvironment)
  , _cietExportToS3Task    :: !(Maybe ExportToS3TaskSpecification)
  , _cietDescription       :: !(Maybe Text)
  , _cietInstanceId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstanceExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cietTargetEnvironment' - The target virtualization environment.
--
-- * 'cietExportToS3Task' - The format and location for an instance export task.
--
-- * 'cietDescription' - A description for the conversion task or the resource being exported. The maximum length is 255 bytes.
--
-- * 'cietInstanceId' - The ID of the instance.
createInstanceExportTask
    :: Text -- ^ 'cietInstanceId'
    -> CreateInstanceExportTask
createInstanceExportTask pInstanceId_ =
  CreateInstanceExportTask'
    { _cietTargetEnvironment = Nothing
    , _cietExportToS3Task = Nothing
    , _cietDescription = Nothing
    , _cietInstanceId = pInstanceId_
    }


-- | The target virtualization environment.
cietTargetEnvironment :: Lens' CreateInstanceExportTask (Maybe ExportEnvironment)
cietTargetEnvironment = lens _cietTargetEnvironment (\ s a -> s{_cietTargetEnvironment = a})

-- | The format and location for an instance export task.
cietExportToS3Task :: Lens' CreateInstanceExportTask (Maybe ExportToS3TaskSpecification)
cietExportToS3Task = lens _cietExportToS3Task (\ s a -> s{_cietExportToS3Task = a})

-- | A description for the conversion task or the resource being exported. The maximum length is 255 bytes.
cietDescription :: Lens' CreateInstanceExportTask (Maybe Text)
cietDescription = lens _cietDescription (\ s a -> s{_cietDescription = a})

-- | The ID of the instance.
cietInstanceId :: Lens' CreateInstanceExportTask Text
cietInstanceId = lens _cietInstanceId (\ s a -> s{_cietInstanceId = a})

instance AWSRequest CreateInstanceExportTask where
        type Rs CreateInstanceExportTask =
             CreateInstanceExportTaskResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateInstanceExportTaskResponse' <$>
                   (x .@? "exportTask") <*> (pure (fromEnum s)))

instance Hashable CreateInstanceExportTask where

instance NFData CreateInstanceExportTask where

instance ToHeaders CreateInstanceExportTask where
        toHeaders = const mempty

instance ToPath CreateInstanceExportTask where
        toPath = const "/"

instance ToQuery CreateInstanceExportTask where
        toQuery CreateInstanceExportTask'{..}
          = mconcat
              ["Action" =:
                 ("CreateInstanceExportTask" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "TargetEnvironment" =: _cietTargetEnvironment,
               "ExportToS3" =: _cietExportToS3Task,
               "Description" =: _cietDescription,
               "InstanceId" =: _cietInstanceId]

-- | Contains the output for CreateInstanceExportTask.
--
--
--
-- /See:/ 'createInstanceExportTaskResponse' smart constructor.
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
  { _cietrsExportTask     :: !(Maybe ExportTask)
  , _cietrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstanceExportTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cietrsExportTask' - Information about the instance export task.
--
-- * 'cietrsResponseStatus' - -- | The response status code.
createInstanceExportTaskResponse
    :: Int -- ^ 'cietrsResponseStatus'
    -> CreateInstanceExportTaskResponse
createInstanceExportTaskResponse pResponseStatus_ =
  CreateInstanceExportTaskResponse'
    {_cietrsExportTask = Nothing, _cietrsResponseStatus = pResponseStatus_}


-- | Information about the instance export task.
cietrsExportTask :: Lens' CreateInstanceExportTaskResponse (Maybe ExportTask)
cietrsExportTask = lens _cietrsExportTask (\ s a -> s{_cietrsExportTask = a})

-- | -- | The response status code.
cietrsResponseStatus :: Lens' CreateInstanceExportTaskResponse Int
cietrsResponseStatus = lens _cietrsResponseStatus (\ s a -> s{_cietrsResponseStatus = a})

instance NFData CreateInstanceExportTaskResponse
         where
