{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.PutActionRevision
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS CodePipeline about new revisions to a
-- source.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_PutActionRevision.html>
module Network.AWS.CodePipeline.PutActionRevision
    (
    -- * Request
      PutActionRevision
    -- ** Request constructor
    , putActionRevision
    -- ** Request lenses
    , parrqPipelineName
    , parrqStageName
    , parrqActionName
    , parrqActionRevision

    -- * Response
    , PutActionRevisionResponse
    -- ** Response constructor
    , putActionRevisionResponse
    -- ** Response lenses
    , parrsNewRevision
    , parrsPipelineExecutionId
    , parrsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a put action revision action.
--
-- /See:/ 'putActionRevision' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'parrqPipelineName'
--
-- * 'parrqStageName'
--
-- * 'parrqActionName'
--
-- * 'parrqActionRevision'
data PutActionRevision = PutActionRevision'
    { _parrqPipelineName   :: !Text
    , _parrqStageName      :: !Text
    , _parrqActionName     :: !Text
    , _parrqActionRevision :: !ActionRevision
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutActionRevision' smart constructor.
putActionRevision :: Text -> Text -> Text -> ActionRevision -> PutActionRevision
putActionRevision pPipelineName pStageName pActionName pActionRevision =
    PutActionRevision'
    { _parrqPipelineName = pPipelineName
    , _parrqStageName = pStageName
    , _parrqActionName = pActionName
    , _parrqActionRevision = pActionRevision
    }

-- | The name of the pipeline that will start processing the revision to the
-- source.
parrqPipelineName :: Lens' PutActionRevision Text
parrqPipelineName = lens _parrqPipelineName (\ s a -> s{_parrqPipelineName = a});

-- | The name of the stage that contains the action that will act upon the
-- revision.
parrqStageName :: Lens' PutActionRevision Text
parrqStageName = lens _parrqStageName (\ s a -> s{_parrqStageName = a});

-- | The name of the action that will process the revision.
parrqActionName :: Lens' PutActionRevision Text
parrqActionName = lens _parrqActionName (\ s a -> s{_parrqActionName = a});

-- | FIXME: Undocumented member.
parrqActionRevision :: Lens' PutActionRevision ActionRevision
parrqActionRevision = lens _parrqActionRevision (\ s a -> s{_parrqActionRevision = a});

instance AWSRequest PutActionRevision where
        type Sv PutActionRevision = CodePipeline
        type Rs PutActionRevision = PutActionRevisionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PutActionRevisionResponse' <$>
                   (x .?> "newRevision") <*>
                     (x .?> "pipelineExecutionId")
                     <*> (pure (fromEnum s)))

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
              ["pipelineName" .= _parrqPipelineName,
               "stageName" .= _parrqStageName,
               "actionName" .= _parrqActionName,
               "actionRevision" .= _parrqActionRevision]

instance ToPath PutActionRevision where
        toPath = const "/"

instance ToQuery PutActionRevision where
        toQuery = const mempty

-- | Represents the output of a put action revision action.
--
-- /See:/ 'putActionRevisionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'parrsNewRevision'
--
-- * 'parrsPipelineExecutionId'
--
-- * 'parrsStatus'
data PutActionRevisionResponse = PutActionRevisionResponse'
    { _parrsNewRevision         :: !(Maybe Bool)
    , _parrsPipelineExecutionId :: !(Maybe Text)
    , _parrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutActionRevisionResponse' smart constructor.
putActionRevisionResponse :: Int -> PutActionRevisionResponse
putActionRevisionResponse pStatus =
    PutActionRevisionResponse'
    { _parrsNewRevision = Nothing
    , _parrsPipelineExecutionId = Nothing
    , _parrsStatus = pStatus
    }

-- | The new revision number or ID for the revision after the action
-- completes.
parrsNewRevision :: Lens' PutActionRevisionResponse (Maybe Bool)
parrsNewRevision = lens _parrsNewRevision (\ s a -> s{_parrsNewRevision = a});

-- | The ID of the current workflow state of the pipeline.
parrsPipelineExecutionId :: Lens' PutActionRevisionResponse (Maybe Text)
parrsPipelineExecutionId = lens _parrsPipelineExecutionId (\ s a -> s{_parrsPipelineExecutionId = a});

-- | FIXME: Undocumented member.
parrsStatus :: Lens' PutActionRevisionResponse Int
parrsStatus = lens _parrsStatus (\ s a -> s{_parrsStatus = a});
