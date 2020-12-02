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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS CodePipeline about new revisions to a source.
--
--
module Network.AWS.CodePipeline.PutActionRevision
    (
    -- * Creating a Request
      putActionRevision
    , PutActionRevision
    -- * Request Lenses
    , pPipelineName
    , pStageName
    , pActionName
    , pActionRevision

    -- * Destructuring the Response
    , putActionRevisionResponse
    , PutActionRevisionResponse
    -- * Response Lenses
    , prsNewRevision
    , prsPipelineExecutionId
    , prsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a PutActionRevision action.
--
--
--
-- /See:/ 'putActionRevision' smart constructor.
data PutActionRevision = PutActionRevision'
  { _pPipelineName   :: !Text
  , _pStageName      :: !Text
  , _pActionName     :: !Text
  , _pActionRevision :: !ActionRevision
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutActionRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPipelineName' - The name of the pipeline that will start processing the revision to the source.
--
-- * 'pStageName' - The name of the stage that contains the action that will act upon the revision.
--
-- * 'pActionName' - The name of the action that will process the revision.
--
-- * 'pActionRevision' - Represents information about the version (or revision) of an action.
putActionRevision
    :: Text -- ^ 'pPipelineName'
    -> Text -- ^ 'pStageName'
    -> Text -- ^ 'pActionName'
    -> ActionRevision -- ^ 'pActionRevision'
    -> PutActionRevision
putActionRevision pPipelineName_ pStageName_ pActionName_ pActionRevision_ =
  PutActionRevision'
    { _pPipelineName = pPipelineName_
    , _pStageName = pStageName_
    , _pActionName = pActionName_
    , _pActionRevision = pActionRevision_
    }


-- | The name of the pipeline that will start processing the revision to the source.
pPipelineName :: Lens' PutActionRevision Text
pPipelineName = lens _pPipelineName (\ s a -> s{_pPipelineName = a})

-- | The name of the stage that contains the action that will act upon the revision.
pStageName :: Lens' PutActionRevision Text
pStageName = lens _pStageName (\ s a -> s{_pStageName = a})

-- | The name of the action that will process the revision.
pActionName :: Lens' PutActionRevision Text
pActionName = lens _pActionName (\ s a -> s{_pActionName = a})

-- | Represents information about the version (or revision) of an action.
pActionRevision :: Lens' PutActionRevision ActionRevision
pActionRevision = lens _pActionRevision (\ s a -> s{_pActionRevision = a})

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

instance Hashable PutActionRevision where

instance NFData PutActionRevision where

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
                 [Just ("pipelineName" .= _pPipelineName),
                  Just ("stageName" .= _pStageName),
                  Just ("actionName" .= _pActionName),
                  Just ("actionRevision" .= _pActionRevision)])

instance ToPath PutActionRevision where
        toPath = const "/"

instance ToQuery PutActionRevision where
        toQuery = const mempty

-- | Represents the output of a PutActionRevision action.
--
--
--
-- /See:/ 'putActionRevisionResponse' smart constructor.
data PutActionRevisionResponse = PutActionRevisionResponse'
  { _prsNewRevision         :: !(Maybe Bool)
  , _prsPipelineExecutionId :: !(Maybe Text)
  , _prsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutActionRevisionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prsNewRevision' - Indicates whether the artifact revision was previously used in an execution of the specified pipeline.
--
-- * 'prsPipelineExecutionId' - The ID of the current workflow state of the pipeline.
--
-- * 'prsResponseStatus' - -- | The response status code.
putActionRevisionResponse
    :: Int -- ^ 'prsResponseStatus'
    -> PutActionRevisionResponse
putActionRevisionResponse pResponseStatus_ =
  PutActionRevisionResponse'
    { _prsNewRevision = Nothing
    , _prsPipelineExecutionId = Nothing
    , _prsResponseStatus = pResponseStatus_
    }


-- | Indicates whether the artifact revision was previously used in an execution of the specified pipeline.
prsNewRevision :: Lens' PutActionRevisionResponse (Maybe Bool)
prsNewRevision = lens _prsNewRevision (\ s a -> s{_prsNewRevision = a})

-- | The ID of the current workflow state of the pipeline.
prsPipelineExecutionId :: Lens' PutActionRevisionResponse (Maybe Text)
prsPipelineExecutionId = lens _prsPipelineExecutionId (\ s a -> s{_prsPipelineExecutionId = a})

-- | -- | The response status code.
prsResponseStatus :: Lens' PutActionRevisionResponse Int
prsResponseStatus = lens _prsResponseStatus (\ s a -> s{_prsResponseStatus = a})

instance NFData PutActionRevisionResponse where
