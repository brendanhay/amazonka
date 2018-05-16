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
-- Module      : Network.AWS.IoT.CreateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job.
--
--
module Network.AWS.IoT.CreateJob
    (
    -- * Creating a Request
      createJob
    , CreateJob
    -- * Request Lenses
    , cjJobExecutionsRolloutConfig
    , cjDocumentSource
    , cjDocumentParameters
    , cjPresignedURLConfig
    , cjDocument
    , cjDescription
    , cjTargetSelection
    , cjJobId
    , cjTargets

    -- * Destructuring the Response
    , createJobResponse
    , CreateJobResponse
    -- * Response Lenses
    , cjrsJobId
    , cjrsJobARN
    , cjrsDescription
    , cjrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createJob' smart constructor.
data CreateJob = CreateJob'
  { _cjJobExecutionsRolloutConfig :: !(Maybe JobExecutionsRolloutConfig)
  , _cjDocumentSource             :: !(Maybe Text)
  , _cjDocumentParameters         :: !(Maybe (Map Text Text))
  , _cjPresignedURLConfig         :: !(Maybe PresignedURLConfig)
  , _cjDocument                   :: !(Maybe Text)
  , _cjDescription                :: !(Maybe Text)
  , _cjTargetSelection            :: !(Maybe TargetSelection)
  , _cjJobId                      :: !Text
  , _cjTargets                    :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjJobExecutionsRolloutConfig' - Allows you to create a staged rollout of the job.
--
-- * 'cjDocumentSource' - An S3 link to the job document.
--
-- * 'cjDocumentParameters' - Parameters for the job document.
--
-- * 'cjPresignedURLConfig' - Configuration information for pre-signed S3 URLs.
--
-- * 'cjDocument' - The job document.
--
-- * 'cjDescription' - A short text description of the job.
--
-- * 'cjTargetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
--
-- * 'cjJobId' - A job identifier which must be unique for your AWS account. We recommend using a UUID. Alpha-numeric characters, "-" and "_" are valid for use here.
--
-- * 'cjTargets' - A list of things and thing groups to which the job should be sent.
createJob
    :: Text -- ^ 'cjJobId'
    -> NonEmpty Text -- ^ 'cjTargets'
    -> CreateJob
createJob pJobId_ pTargets_ =
  CreateJob'
    { _cjJobExecutionsRolloutConfig = Nothing
    , _cjDocumentSource = Nothing
    , _cjDocumentParameters = Nothing
    , _cjPresignedURLConfig = Nothing
    , _cjDocument = Nothing
    , _cjDescription = Nothing
    , _cjTargetSelection = Nothing
    , _cjJobId = pJobId_
    , _cjTargets = _List1 # pTargets_
    }


-- | Allows you to create a staged rollout of the job.
cjJobExecutionsRolloutConfig :: Lens' CreateJob (Maybe JobExecutionsRolloutConfig)
cjJobExecutionsRolloutConfig = lens _cjJobExecutionsRolloutConfig (\ s a -> s{_cjJobExecutionsRolloutConfig = a})

-- | An S3 link to the job document.
cjDocumentSource :: Lens' CreateJob (Maybe Text)
cjDocumentSource = lens _cjDocumentSource (\ s a -> s{_cjDocumentSource = a})

-- | Parameters for the job document.
cjDocumentParameters :: Lens' CreateJob (HashMap Text Text)
cjDocumentParameters = lens _cjDocumentParameters (\ s a -> s{_cjDocumentParameters = a}) . _Default . _Map

-- | Configuration information for pre-signed S3 URLs.
cjPresignedURLConfig :: Lens' CreateJob (Maybe PresignedURLConfig)
cjPresignedURLConfig = lens _cjPresignedURLConfig (\ s a -> s{_cjPresignedURLConfig = a})

-- | The job document.
cjDocument :: Lens' CreateJob (Maybe Text)
cjDocument = lens _cjDocument (\ s a -> s{_cjDocument = a})

-- | A short text description of the job.
cjDescription :: Lens' CreateJob (Maybe Text)
cjDescription = lens _cjDescription (\ s a -> s{_cjDescription = a})

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
cjTargetSelection :: Lens' CreateJob (Maybe TargetSelection)
cjTargetSelection = lens _cjTargetSelection (\ s a -> s{_cjTargetSelection = a})

-- | A job identifier which must be unique for your AWS account. We recommend using a UUID. Alpha-numeric characters, "-" and "_" are valid for use here.
cjJobId :: Lens' CreateJob Text
cjJobId = lens _cjJobId (\ s a -> s{_cjJobId = a})

-- | A list of things and thing groups to which the job should be sent.
cjTargets :: Lens' CreateJob (NonEmpty Text)
cjTargets = lens _cjTargets (\ s a -> s{_cjTargets = a}) . _List1

instance AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        request = putJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateJobResponse' <$>
                   (x .?> "jobId") <*> (x .?> "jobArn") <*>
                     (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable CreateJob where

instance NFData CreateJob where

instance ToHeaders CreateJob where
        toHeaders = const mempty

instance ToJSON CreateJob where
        toJSON CreateJob'{..}
          = object
              (catMaybes
                 [("jobExecutionsRolloutConfig" .=) <$>
                    _cjJobExecutionsRolloutConfig,
                  ("documentSource" .=) <$> _cjDocumentSource,
                  ("documentParameters" .=) <$> _cjDocumentParameters,
                  ("presignedUrlConfig" .=) <$> _cjPresignedURLConfig,
                  ("document" .=) <$> _cjDocument,
                  ("description" .=) <$> _cjDescription,
                  ("targetSelection" .=) <$> _cjTargetSelection,
                  Just ("targets" .= _cjTargets)])

instance ToPath CreateJob where
        toPath CreateJob'{..}
          = mconcat ["/jobs/", toBS _cjJobId]

instance ToQuery CreateJob where
        toQuery = const mempty

-- | /See:/ 'createJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { _cjrsJobId          :: !(Maybe Text)
  , _cjrsJobARN         :: !(Maybe Text)
  , _cjrsDescription    :: !(Maybe Text)
  , _cjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsJobId' - The unique identifier you assigned to this job.
--
-- * 'cjrsJobARN' - The job ARN.
--
-- * 'cjrsDescription' - The job description.
--
-- * 'cjrsResponseStatus' - -- | The response status code.
createJobResponse
    :: Int -- ^ 'cjrsResponseStatus'
    -> CreateJobResponse
createJobResponse pResponseStatus_ =
  CreateJobResponse'
    { _cjrsJobId = Nothing
    , _cjrsJobARN = Nothing
    , _cjrsDescription = Nothing
    , _cjrsResponseStatus = pResponseStatus_
    }


-- | The unique identifier you assigned to this job.
cjrsJobId :: Lens' CreateJobResponse (Maybe Text)
cjrsJobId = lens _cjrsJobId (\ s a -> s{_cjrsJobId = a})

-- | The job ARN.
cjrsJobARN :: Lens' CreateJobResponse (Maybe Text)
cjrsJobARN = lens _cjrsJobARN (\ s a -> s{_cjrsJobARN = a})

-- | The job description.
cjrsDescription :: Lens' CreateJobResponse (Maybe Text)
cjrsDescription = lens _cjrsDescription (\ s a -> s{_cjrsDescription = a})

-- | -- | The response status code.
cjrsResponseStatus :: Lens' CreateJobResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\ s a -> s{_cjrsResponseStatus = a})

instance NFData CreateJobResponse where
