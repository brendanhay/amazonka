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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a running or stopped instance to an S3 bucket.
--
-- For information about the supported operating systems, image formats,
-- and known limitations for the types of instances you can export, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ExportingEC2Instances.html Exporting EC2 Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInstanceExportTask.html AWS API Reference> for CreateInstanceExportTask.
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
    , cietrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createInstanceExportTask' smart constructor.
data CreateInstanceExportTask = CreateInstanceExportTask'
    { _cietTargetEnvironment :: !(Maybe ExportEnvironment)
    , _cietExportToS3Task    :: !(Maybe ExportToS3TaskSpecification)
    , _cietDescription       :: !(Maybe Text)
    , _cietInstanceId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateInstanceExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cietTargetEnvironment'
--
-- * 'cietExportToS3Task'
--
-- * 'cietDescription'
--
-- * 'cietInstanceId'
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
cietTargetEnvironment = lens _cietTargetEnvironment (\ s a -> s{_cietTargetEnvironment = a});

-- | The format and location for an instance export task.
cietExportToS3Task :: Lens' CreateInstanceExportTask (Maybe ExportToS3TaskSpecification)
cietExportToS3Task = lens _cietExportToS3Task (\ s a -> s{_cietExportToS3Task = a});

-- | A description for the conversion task or the resource being exported.
-- The maximum length is 255 bytes.
cietDescription :: Lens' CreateInstanceExportTask (Maybe Text)
cietDescription = lens _cietDescription (\ s a -> s{_cietDescription = a});

-- | The ID of the instance.
cietInstanceId :: Lens' CreateInstanceExportTask Text
cietInstanceId = lens _cietInstanceId (\ s a -> s{_cietInstanceId = a});

instance AWSRequest CreateInstanceExportTask where
        type Rs CreateInstanceExportTask =
             CreateInstanceExportTaskResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 CreateInstanceExportTaskResponse' <$>
                   (x .@? "exportTask") <*> (pure (fromEnum s)))

instance ToHeaders CreateInstanceExportTask where
        toHeaders = const mempty

instance ToPath CreateInstanceExportTask where
        toPath = const "/"

instance ToQuery CreateInstanceExportTask where
        toQuery CreateInstanceExportTask'{..}
          = mconcat
              ["Action" =:
                 ("CreateInstanceExportTask" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "TargetEnvironment" =: _cietTargetEnvironment,
               "ExportToS3" =: _cietExportToS3Task,
               "Description" =: _cietDescription,
               "InstanceId" =: _cietInstanceId]

-- | /See:/ 'createInstanceExportTaskResponse' smart constructor.
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
    { _cietrsExportTask :: !(Maybe ExportTask)
    , _cietrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateInstanceExportTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cietrsExportTask'
--
-- * 'cietrsStatus'
createInstanceExportTaskResponse
    :: Int -- ^ 'cietrsStatus'
    -> CreateInstanceExportTaskResponse
createInstanceExportTaskResponse pStatus_ =
    CreateInstanceExportTaskResponse'
    { _cietrsExportTask = Nothing
    , _cietrsStatus = pStatus_
    }

-- | Information about the instance export task.
cietrsExportTask :: Lens' CreateInstanceExportTaskResponse (Maybe ExportTask)
cietrsExportTask = lens _cietrsExportTask (\ s a -> s{_cietrsExportTask = a});

-- | The response status code.
cietrsStatus :: Lens' CreateInstanceExportTaskResponse Int
cietrsStatus = lens _cietrsStatus (\ s a -> s{_cietrsStatus = a});
