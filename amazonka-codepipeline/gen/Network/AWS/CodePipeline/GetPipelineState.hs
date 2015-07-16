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
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_GetPipelineState.html>
module Network.AWS.CodePipeline.GetPipelineState
    (
    -- * Request
      GetPipelineState
    -- ** Request constructor
    , getPipelineState
    -- ** Request lenses
    , gpsName

    -- * Response
    , GetPipelineStateResponse
    -- ** Response constructor
    , getPipelineStateResponse
    -- ** Response lenses
    , gpsrPipelineName
    , gpsrPipelineVersion
    , gpsrCreated
    , gpsrStageStates
    , gpsrUpdated
    , gpsrStatus
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
getPipelineState pName =
    GetPipelineState'
    { _gpsName = pName
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
-- * 'gpsrPipelineName'
--
-- * 'gpsrPipelineVersion'
--
-- * 'gpsrCreated'
--
-- * 'gpsrStageStates'
--
-- * 'gpsrUpdated'
--
-- * 'gpsrStatus'
data GetPipelineStateResponse = GetPipelineStateResponse'
    { _gpsrPipelineName    :: !(Maybe Text)
    , _gpsrPipelineVersion :: !(Maybe Nat)
    , _gpsrCreated         :: !(Maybe POSIX)
    , _gpsrStageStates     :: !(Maybe [StageState])
    , _gpsrUpdated         :: !(Maybe POSIX)
    , _gpsrStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPipelineStateResponse' smart constructor.
getPipelineStateResponse :: Int -> GetPipelineStateResponse
getPipelineStateResponse pStatus =
    GetPipelineStateResponse'
    { _gpsrPipelineName = Nothing
    , _gpsrPipelineVersion = Nothing
    , _gpsrCreated = Nothing
    , _gpsrStageStates = Nothing
    , _gpsrUpdated = Nothing
    , _gpsrStatus = pStatus
    }

-- | The name of the pipeline for which you want to get the state.
gpsrPipelineName :: Lens' GetPipelineStateResponse (Maybe Text)
gpsrPipelineName = lens _gpsrPipelineName (\ s a -> s{_gpsrPipelineName = a});

-- | The version number of the pipeline.
--
-- A newly-created pipeline is always assigned a version number of @1@.
gpsrPipelineVersion :: Lens' GetPipelineStateResponse (Maybe Natural)
gpsrPipelineVersion = lens _gpsrPipelineVersion (\ s a -> s{_gpsrPipelineVersion = a}) . mapping _Nat;

-- | The date and time the pipeline was created, in timestamp format.
gpsrCreated :: Lens' GetPipelineStateResponse (Maybe UTCTime)
gpsrCreated = lens _gpsrCreated (\ s a -> s{_gpsrCreated = a}) . mapping _Time;

-- | A list of the pipeline stage output information, including stage name,
-- state, most recent run details, whether the stage is disabled, and other
-- data.
gpsrStageStates :: Lens' GetPipelineStateResponse [StageState]
gpsrStageStates = lens _gpsrStageStates (\ s a -> s{_gpsrStageStates = a}) . _Default;

-- | The date and time the pipeline was last updated, in timestamp format.
gpsrUpdated :: Lens' GetPipelineStateResponse (Maybe UTCTime)
gpsrUpdated = lens _gpsrUpdated (\ s a -> s{_gpsrUpdated = a}) . mapping _Time;

-- | FIXME: Undocumented member.
gpsrStatus :: Lens' GetPipelineStateResponse Int
gpsrStatus = lens _gpsrStatus (\ s a -> s{_gpsrStatus = a});
