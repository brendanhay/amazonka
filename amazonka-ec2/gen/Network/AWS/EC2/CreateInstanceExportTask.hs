{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateInstanceExportTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Exports a running or stopped instance to an S3 bucket.
--
-- For information about the supported operating systems, image formats,
-- and known limitations for the types of instances you can export, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ExportingEC2Instances.html Exporting EC2 Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateInstanceExportTask.html>
module Network.AWS.EC2.CreateInstanceExportTask
    (
    -- * Request
      CreateInstanceExportTask
    -- ** Request constructor
    , createInstanceExportTask
    -- ** Request lenses
    , cietrqTargetEnvironment
    , cietrqExportToS3Task
    , cietrqDescription
    , cietrqInstanceId

    -- * Response
    , CreateInstanceExportTaskResponse
    -- ** Response constructor
    , createInstanceExportTaskResponse
    -- ** Response lenses
    , cietrsExportTask
    , cietrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createInstanceExportTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cietrqTargetEnvironment'
--
-- * 'cietrqExportToS3Task'
--
-- * 'cietrqDescription'
--
-- * 'cietrqInstanceId'
data CreateInstanceExportTask = CreateInstanceExportTask'
    { _cietrqTargetEnvironment :: !(Maybe ExportEnvironment)
    , _cietrqExportToS3Task    :: !(Maybe ExportToS3TaskSpecification)
    , _cietrqDescription       :: !(Maybe Text)
    , _cietrqInstanceId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInstanceExportTask' smart constructor.
createInstanceExportTask :: Text -> CreateInstanceExportTask
createInstanceExportTask pInstanceId =
    CreateInstanceExportTask'
    { _cietrqTargetEnvironment = Nothing
    , _cietrqExportToS3Task = Nothing
    , _cietrqDescription = Nothing
    , _cietrqInstanceId = pInstanceId
    }

-- | The target virtualization environment.
cietrqTargetEnvironment :: Lens' CreateInstanceExportTask (Maybe ExportEnvironment)
cietrqTargetEnvironment = lens _cietrqTargetEnvironment (\ s a -> s{_cietrqTargetEnvironment = a});

-- | The format and location for an instance export task.
cietrqExportToS3Task :: Lens' CreateInstanceExportTask (Maybe ExportToS3TaskSpecification)
cietrqExportToS3Task = lens _cietrqExportToS3Task (\ s a -> s{_cietrqExportToS3Task = a});

-- | A description for the conversion task or the resource being exported.
-- The maximum length is 255 bytes.
cietrqDescription :: Lens' CreateInstanceExportTask (Maybe Text)
cietrqDescription = lens _cietrqDescription (\ s a -> s{_cietrqDescription = a});

-- | The ID of the instance.
cietrqInstanceId :: Lens' CreateInstanceExportTask Text
cietrqInstanceId = lens _cietrqInstanceId (\ s a -> s{_cietrqInstanceId = a});

instance AWSRequest CreateInstanceExportTask where
        type Sv CreateInstanceExportTask = EC2
        type Rs CreateInstanceExportTask =
             CreateInstanceExportTaskResponse
        request = post
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
               "TargetEnvironment" =: _cietrqTargetEnvironment,
               "ExportToS3" =: _cietrqExportToS3Task,
               "Description" =: _cietrqDescription,
               "InstanceId" =: _cietrqInstanceId]

-- | /See:/ 'createInstanceExportTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cietrsExportTask'
--
-- * 'cietrsStatus'
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
    { _cietrsExportTask :: !(Maybe ExportTask)
    , _cietrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInstanceExportTaskResponse' smart constructor.
createInstanceExportTaskResponse :: Int -> CreateInstanceExportTaskResponse
createInstanceExportTaskResponse pStatus =
    CreateInstanceExportTaskResponse'
    { _cietrsExportTask = Nothing
    , _cietrsStatus = pStatus
    }

-- | Information about the instance export task.
cietrsExportTask :: Lens' CreateInstanceExportTaskResponse (Maybe ExportTask)
cietrsExportTask = lens _cietrsExportTask (\ s a -> s{_cietrsExportTask = a});

-- | FIXME: Undocumented member.
cietrsStatus :: Lens' CreateInstanceExportTaskResponse Int
cietrsStatus = lens _cietrsStatus (\ s a -> s{_cietrsStatus = a});
