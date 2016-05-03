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
-- Module      : Network.AWS.CodePipeline.PutActionRevision
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS CodePipeline about new revisions to a
-- source.
module Network.AWS.CodePipeline.PutActionRevision
    (
    -- * Creating a Request
      putActionRevision
    , PutActionRevision
    -- * Request Lenses
    , parPipelineName
    , parStageName
    , parActionName
    , parActionRevision

    -- * Destructuring the Response
    , putActionRevisionResponse
    , PutActionRevisionResponse
    -- * Response Lenses
    , parrsNewRevision
    , parrsPipelineExecutionId
    , parrsResponseStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.CodePipeline.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a put action revision action.
--
-- /See:/ 'putActionRevision' smart constructor.
data PutActionRevision = PutActionRevision'
    { _parPipelineName   :: !Text
    , _parStageName      :: !Text
    , _parActionName     :: !Text
    , _parActionRevision :: !ActionRevision
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutActionRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parPipelineName'
--
-- * 'parStageName'
--
-- * 'parActionName'
--
-- * 'parActionRevision'
putActionRevision
    :: Text -- ^ 'parPipelineName'
    -> Text -- ^ 'parStageName'
    -> Text -- ^ 'parActionName'
    -> ActionRevision -- ^ 'parActionRevision'
    -> PutActionRevision
putActionRevision pPipelineName_ pStageName_ pActionName_ pActionRevision_ =
    PutActionRevision'
    { _parPipelineName = pPipelineName_
    , _parStageName = pStageName_
    , _parActionName = pActionName_
    , _parActionRevision = pActionRevision_
    }

-- | The name of the pipeline that will start processing the revision to the
-- source.
parPipelineName :: Lens' PutActionRevision Text
parPipelineName = lens _parPipelineName (\ s a -> s{_parPipelineName = a});

-- | The name of the stage that contains the action that will act upon the
-- revision.
parStageName :: Lens' PutActionRevision Text
parStageName = lens _parStageName (\ s a -> s{_parStageName = a});

-- | The name of the action that will process the revision.
parActionName :: Lens' PutActionRevision Text
parActionName = lens _parActionName (\ s a -> s{_parActionName = a});

-- | Undocumented member.
parActionRevision :: Lens' PutActionRevision ActionRevision
parActionRevision = lens _parActionRevision (\ s a -> s{_parActionRevision = a});

instance AWSRequest PutActionRevision where
        type Rs PutActionRevision = PutActionRevisionResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 PutActionRevisionResponse' <$>
                   (x .?> "newRevision") <*>
                     (x .?> "pipelineExecutionId")
                     <*> (pure (fromEnum s)))

instance Hashable PutActionRevision

instance NFData PutActionRevision

instance ToHeaders PutActionRevision where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.PutActionRevision" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutActionRevision where
        toJSON PutActionRevision'{..}
          = object
              (catMaybes
                 [Just ("pipelineName" .= _parPipelineName),
                  Just ("stageName" .= _parStageName),
                  Just ("actionName" .= _parActionName),
                  Just ("actionRevision" .= _parActionRevision)])

instance ToPath PutActionRevision where
        toPath = const "/"

instance ToQuery PutActionRevision where
        toQuery = const mempty

-- | Represents the output of a put action revision action.
--
-- /See:/ 'putActionRevisionResponse' smart constructor.
data PutActionRevisionResponse = PutActionRevisionResponse'
    { _parrsNewRevision         :: !(Maybe Bool)
    , _parrsPipelineExecutionId :: !(Maybe Text)
    , _parrsResponseStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutActionRevisionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'parrsNewRevision'
--
-- * 'parrsPipelineExecutionId'
--
-- * 'parrsResponseStatus'
putActionRevisionResponse
    :: Int -- ^ 'parrsResponseStatus'
    -> PutActionRevisionResponse
putActionRevisionResponse pResponseStatus_ =
    PutActionRevisionResponse'
    { _parrsNewRevision = Nothing
    , _parrsPipelineExecutionId = Nothing
    , _parrsResponseStatus = pResponseStatus_
    }

-- | The new revision number or ID for the revision after the action
-- completes.
parrsNewRevision :: Lens' PutActionRevisionResponse (Maybe Bool)
parrsNewRevision = lens _parrsNewRevision (\ s a -> s{_parrsNewRevision = a});

-- | The ID of the current workflow state of the pipeline.
parrsPipelineExecutionId :: Lens' PutActionRevisionResponse (Maybe Text)
parrsPipelineExecutionId = lens _parrsPipelineExecutionId (\ s a -> s{_parrsPipelineExecutionId = a});

-- | The response status code.
parrsResponseStatus :: Lens' PutActionRevisionResponse Int
parrsResponseStatus = lens _parrsResponseStatus (\ s a -> s{_parrsResponseStatus = a});

instance NFData PutActionRevisionResponse
