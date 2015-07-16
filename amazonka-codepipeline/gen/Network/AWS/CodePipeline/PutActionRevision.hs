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
    , parPipelineName
    , parStageName
    , parActionName
    , parActionRevision

    -- * Response
    , PutActionRevisionResponse
    -- ** Response constructor
    , putActionRevisionResponse
    -- ** Response lenses
    , parrNewRevision
    , parrPipelineExecutionId
    , parrStatus
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
-- * 'parPipelineName'
--
-- * 'parStageName'
--
-- * 'parActionName'
--
-- * 'parActionRevision'
data PutActionRevision = PutActionRevision'
    { _parPipelineName   :: !Text
    , _parStageName      :: !Text
    , _parActionName     :: !Text
    , _parActionRevision :: !ActionRevision
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutActionRevision' smart constructor.
putActionRevision :: Text -> Text -> Text -> ActionRevision -> PutActionRevision
putActionRevision pPipelineName pStageName pActionName pActionRevision =
    PutActionRevision'
    { _parPipelineName = pPipelineName
    , _parStageName = pStageName
    , _parActionName = pActionName
    , _parActionRevision = pActionRevision
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

-- | FIXME: Undocumented member.
parActionRevision :: Lens' PutActionRevision ActionRevision
parActionRevision = lens _parActionRevision (\ s a -> s{_parActionRevision = a});

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
              ["pipelineName" .= _parPipelineName,
               "stageName" .= _parStageName,
               "actionName" .= _parActionName,
               "actionRevision" .= _parActionRevision]

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
-- * 'parrNewRevision'
--
-- * 'parrPipelineExecutionId'
--
-- * 'parrStatus'
data PutActionRevisionResponse = PutActionRevisionResponse'
    { _parrNewRevision         :: !(Maybe Bool)
    , _parrPipelineExecutionId :: !(Maybe Text)
    , _parrStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutActionRevisionResponse' smart constructor.
putActionRevisionResponse :: Int -> PutActionRevisionResponse
putActionRevisionResponse pStatus =
    PutActionRevisionResponse'
    { _parrNewRevision = Nothing
    , _parrPipelineExecutionId = Nothing
    , _parrStatus = pStatus
    }

-- | The new revision number or ID for the revision after the action
-- completes.
parrNewRevision :: Lens' PutActionRevisionResponse (Maybe Bool)
parrNewRevision = lens _parrNewRevision (\ s a -> s{_parrNewRevision = a});

-- | The ID of the current workflow state of the pipeline.
parrPipelineExecutionId :: Lens' PutActionRevisionResponse (Maybe Text)
parrPipelineExecutionId = lens _parrPipelineExecutionId (\ s a -> s{_parrPipelineExecutionId = a});

-- | FIXME: Undocumented member.
parrStatus :: Lens' PutActionRevisionResponse Int
parrStatus = lens _parrStatus (\ s a -> s{_parrStatus = a});
