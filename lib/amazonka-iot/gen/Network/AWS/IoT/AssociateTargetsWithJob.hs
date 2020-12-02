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
-- Module      : Network.AWS.IoT.AssociateTargetsWithJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a group with a continuous job. The following criteria must be met:
--
--
--     * The job must have been created with the @targetSelection@ field set to "CONTINUOUS".
--
--     * The job status must currently be "IN_PROGRESS".
--
--     * The total number of targets associated with a job must not exceed 100.
--
--
--
module Network.AWS.IoT.AssociateTargetsWithJob
    (
    -- * Creating a Request
      associateTargetsWithJob
    , AssociateTargetsWithJob
    -- * Request Lenses
    , atwjComment
    , atwjTargets
    , atwjJobId

    -- * Destructuring the Response
    , associateTargetsWithJobResponse
    , AssociateTargetsWithJobResponse
    -- * Response Lenses
    , atwjrsJobId
    , atwjrsJobARN
    , atwjrsDescription
    , atwjrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateTargetsWithJob' smart constructor.
data AssociateTargetsWithJob = AssociateTargetsWithJob'
  { _atwjComment :: !(Maybe Text)
  , _atwjTargets :: !(List1 Text)
  , _atwjJobId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateTargetsWithJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atwjComment' - An optional comment string describing why the job was associated with the targets.
--
-- * 'atwjTargets' - A list of thing group ARNs that define the targets of the job.
--
-- * 'atwjJobId' - The unique identifier you assigned to this job when it was created.
associateTargetsWithJob
    :: NonEmpty Text -- ^ 'atwjTargets'
    -> Text -- ^ 'atwjJobId'
    -> AssociateTargetsWithJob
associateTargetsWithJob pTargets_ pJobId_ =
  AssociateTargetsWithJob'
    { _atwjComment = Nothing
    , _atwjTargets = _List1 # pTargets_
    , _atwjJobId = pJobId_
    }


-- | An optional comment string describing why the job was associated with the targets.
atwjComment :: Lens' AssociateTargetsWithJob (Maybe Text)
atwjComment = lens _atwjComment (\ s a -> s{_atwjComment = a})

-- | A list of thing group ARNs that define the targets of the job.
atwjTargets :: Lens' AssociateTargetsWithJob (NonEmpty Text)
atwjTargets = lens _atwjTargets (\ s a -> s{_atwjTargets = a}) . _List1

-- | The unique identifier you assigned to this job when it was created.
atwjJobId :: Lens' AssociateTargetsWithJob Text
atwjJobId = lens _atwjJobId (\ s a -> s{_atwjJobId = a})

instance AWSRequest AssociateTargetsWithJob where
        type Rs AssociateTargetsWithJob =
             AssociateTargetsWithJobResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 AssociateTargetsWithJobResponse' <$>
                   (x .?> "jobId") <*> (x .?> "jobArn") <*>
                     (x .?> "description")
                     <*> (pure (fromEnum s)))

instance Hashable AssociateTargetsWithJob where

instance NFData AssociateTargetsWithJob where

instance ToHeaders AssociateTargetsWithJob where
        toHeaders = const mempty

instance ToJSON AssociateTargetsWithJob where
        toJSON AssociateTargetsWithJob'{..}
          = object
              (catMaybes
                 [("comment" .=) <$> _atwjComment,
                  Just ("targets" .= _atwjTargets)])

instance ToPath AssociateTargetsWithJob where
        toPath AssociateTargetsWithJob'{..}
          = mconcat ["/jobs/", toBS _atwjJobId, "/targets"]

instance ToQuery AssociateTargetsWithJob where
        toQuery = const mempty

-- | /See:/ 'associateTargetsWithJobResponse' smart constructor.
data AssociateTargetsWithJobResponse = AssociateTargetsWithJobResponse'
  { _atwjrsJobId          :: !(Maybe Text)
  , _atwjrsJobARN         :: !(Maybe Text)
  , _atwjrsDescription    :: !(Maybe Text)
  , _atwjrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateTargetsWithJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atwjrsJobId' - The unique identifier you assigned to this job when it was created.
--
-- * 'atwjrsJobARN' - An ARN identifying the job.
--
-- * 'atwjrsDescription' - A short text description of the job.
--
-- * 'atwjrsResponseStatus' - -- | The response status code.
associateTargetsWithJobResponse
    :: Int -- ^ 'atwjrsResponseStatus'
    -> AssociateTargetsWithJobResponse
associateTargetsWithJobResponse pResponseStatus_ =
  AssociateTargetsWithJobResponse'
    { _atwjrsJobId = Nothing
    , _atwjrsJobARN = Nothing
    , _atwjrsDescription = Nothing
    , _atwjrsResponseStatus = pResponseStatus_
    }


-- | The unique identifier you assigned to this job when it was created.
atwjrsJobId :: Lens' AssociateTargetsWithJobResponse (Maybe Text)
atwjrsJobId = lens _atwjrsJobId (\ s a -> s{_atwjrsJobId = a})

-- | An ARN identifying the job.
atwjrsJobARN :: Lens' AssociateTargetsWithJobResponse (Maybe Text)
atwjrsJobARN = lens _atwjrsJobARN (\ s a -> s{_atwjrsJobARN = a})

-- | A short text description of the job.
atwjrsDescription :: Lens' AssociateTargetsWithJobResponse (Maybe Text)
atwjrsDescription = lens _atwjrsDescription (\ s a -> s{_atwjrsDescription = a})

-- | -- | The response status code.
atwjrsResponseStatus :: Lens' AssociateTargetsWithJobResponse Int
atwjrsResponseStatus = lens _atwjrsResponseStatus (\ s a -> s{_atwjrsResponseStatus = a})

instance NFData AssociateTargetsWithJobResponse where
