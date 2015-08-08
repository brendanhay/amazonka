{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.GetPipelineState
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the state of a pipeline, including the stages,
-- actions, and details about the last run of the pipeline.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_GetPipelineState.html AWS API Reference> for GetPipelineState.
module Network.AWS.CodePipeline.GetPipelineState
    (
    -- * Creating a Request
      GetPipelineState
    , getPipelineState
    -- * Request Lenses
    , gpsName

    -- * Destructuring the Response
    , GetPipelineStateResponse
    , getPipelineStateResponse
    -- * Response Lenses
    , gpsrsPipelineName
    , gpsrsPipelineVersion
    , gpsrsCreated
    , gpsrsStageStates
    , gpsrsUpdated
    , gpsrsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get pipeline state action.
--
-- /See:/ 'getPipelineState' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpsName'
newtype GetPipelineState = GetPipelineState'
    { _gpsName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPipelineState' smart constructor.
getPipelineState :: Text -> GetPipelineState
getPipelineState pName_ =
    GetPipelineState'
    { _gpsName = pName_
    }

-- | The name of the pipeline about which you want to get information.
gpsName :: Lens' GetPipelineState Text
gpsName = lens _gpsName (\ s a -> s{_gpsName = a});

instance AWSRequest GetPipelineState where
        type Sv GetPipelineState = CodePipeline
        type Rs GetPipelineState = GetPipelineStateResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetPipelineStateResponse' <$>
                   (x .?> "pipelineName") <*> (x .?> "pipelineVersion")
                     <*> (x .?> "created")
                     <*> (x .?> "stageStates" .!@ mempty)
                     <*> (x .?> "updated")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetPipelineState where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.GetPipelineState" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPipelineState where
        toJSON GetPipelineState'{..}
          = object ["name" .= _gpsName]

instance ToPath GetPipelineState where
        toPath = const "/"

instance ToQuery GetPipelineState where
        toQuery = const mempty

-- | Represents the output of a get pipeline state action.
--
-- /See:/ 'getPipelineStateResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpsrsPipelineName'
--
-- * 'gpsrsPipelineVersion'
--
-- * 'gpsrsCreated'
--
-- * 'gpsrsStageStates'
--
-- * 'gpsrsUpdated'
--
-- * 'gpsrsStatus'
data GetPipelineStateResponse = GetPipelineStateResponse'
    { _gpsrsPipelineName    :: !(Maybe Text)
    , _gpsrsPipelineVersion :: !(Maybe Nat)
    , _gpsrsCreated         :: !(Maybe POSIX)
    , _gpsrsStageStates     :: !(Maybe [StageState])
    , _gpsrsUpdated         :: !(Maybe POSIX)
    , _gpsrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPipelineStateResponse' smart constructor.
getPipelineStateResponse :: Int -> GetPipelineStateResponse
getPipelineStateResponse pStatus_ =
    GetPipelineStateResponse'
    { _gpsrsPipelineName = Nothing
    , _gpsrsPipelineVersion = Nothing
    , _gpsrsCreated = Nothing
    , _gpsrsStageStates = Nothing
    , _gpsrsUpdated = Nothing
    , _gpsrsStatus = pStatus_
    }

-- | The name of the pipeline for which you want to get the state.
gpsrsPipelineName :: Lens' GetPipelineStateResponse (Maybe Text)
gpsrsPipelineName = lens _gpsrsPipelineName (\ s a -> s{_gpsrsPipelineName = a});

-- | The version number of the pipeline.
--
-- A newly-created pipeline is always assigned a version number of @1@.
gpsrsPipelineVersion :: Lens' GetPipelineStateResponse (Maybe Natural)
gpsrsPipelineVersion = lens _gpsrsPipelineVersion (\ s a -> s{_gpsrsPipelineVersion = a}) . mapping _Nat;

-- | The date and time the pipeline was created, in timestamp format.
gpsrsCreated :: Lens' GetPipelineStateResponse (Maybe UTCTime)
gpsrsCreated = lens _gpsrsCreated (\ s a -> s{_gpsrsCreated = a}) . mapping _Time;

-- | A list of the pipeline stage output information, including stage name,
-- state, most recent run details, whether the stage is disabled, and other
-- data.
gpsrsStageStates :: Lens' GetPipelineStateResponse [StageState]
gpsrsStageStates = lens _gpsrsStageStates (\ s a -> s{_gpsrsStageStates = a}) . _Default . _Coerce;

-- | The date and time the pipeline was last updated, in timestamp format.
gpsrsUpdated :: Lens' GetPipelineStateResponse (Maybe UTCTime)
gpsrsUpdated = lens _gpsrsUpdated (\ s a -> s{_gpsrsUpdated = a}) . mapping _Time;

-- | Undocumented member.
gpsrsStatus :: Lens' GetPipelineStateResponse Int
gpsrsStatus = lens _gpsrsStatus (\ s a -> s{_gpsrsStatus = a});
