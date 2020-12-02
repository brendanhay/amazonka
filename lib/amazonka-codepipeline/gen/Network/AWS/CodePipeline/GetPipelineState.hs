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
-- Module      : Network.AWS.CodePipeline.GetPipelineState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the state of a pipeline, including the stages and actions.
--
--
module Network.AWS.CodePipeline.GetPipelineState
    (
    -- * Creating a Request
      getPipelineState
    , GetPipelineState
    -- * Request Lenses
    , gpsName

    -- * Destructuring the Response
    , getPipelineStateResponse
    , GetPipelineStateResponse
    -- * Response Lenses
    , gpsrsPipelineName
    , gpsrsCreated
    , gpsrsStageStates
    , gpsrsPipelineVersion
    , gpsrsUpdated
    , gpsrsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a GetPipelineState action.
--
--
--
-- /See:/ 'getPipelineState' smart constructor.
newtype GetPipelineState = GetPipelineState'
  { _gpsName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPipelineState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpsName' - The name of the pipeline about which you want to get information.
getPipelineState
    :: Text -- ^ 'gpsName'
    -> GetPipelineState
getPipelineState pName_ = GetPipelineState' {_gpsName = pName_}


-- | The name of the pipeline about which you want to get information.
gpsName :: Lens' GetPipelineState Text
gpsName = lens _gpsName (\ s a -> s{_gpsName = a})

instance AWSRequest GetPipelineState where
        type Rs GetPipelineState = GetPipelineStateResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 GetPipelineStateResponse' <$>
                   (x .?> "pipelineName") <*> (x .?> "created") <*>
                     (x .?> "stageStates" .!@ mempty)
                     <*> (x .?> "pipelineVersion")
                     <*> (x .?> "updated")
                     <*> (pure (fromEnum s)))

instance Hashable GetPipelineState where

instance NFData GetPipelineState where

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
          = object (catMaybes [Just ("name" .= _gpsName)])

instance ToPath GetPipelineState where
        toPath = const "/"

instance ToQuery GetPipelineState where
        toQuery = const mempty

-- | Represents the output of a GetPipelineState action.
--
--
--
-- /See:/ 'getPipelineStateResponse' smart constructor.
data GetPipelineStateResponse = GetPipelineStateResponse'
  { _gpsrsPipelineName    :: !(Maybe Text)
  , _gpsrsCreated         :: !(Maybe POSIX)
  , _gpsrsStageStates     :: !(Maybe [StageState])
  , _gpsrsPipelineVersion :: !(Maybe Nat)
  , _gpsrsUpdated         :: !(Maybe POSIX)
  , _gpsrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPipelineStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpsrsPipelineName' - The name of the pipeline for which you want to get the state.
--
-- * 'gpsrsCreated' - The date and time the pipeline was created, in timestamp format.
--
-- * 'gpsrsStageStates' - A list of the pipeline stage output information, including stage name, state, most recent run details, whether the stage is disabled, and other data.
--
-- * 'gpsrsPipelineVersion' - The version number of the pipeline.
--
-- * 'gpsrsUpdated' - The date and time the pipeline was last updated, in timestamp format.
--
-- * 'gpsrsResponseStatus' - -- | The response status code.
getPipelineStateResponse
    :: Int -- ^ 'gpsrsResponseStatus'
    -> GetPipelineStateResponse
getPipelineStateResponse pResponseStatus_ =
  GetPipelineStateResponse'
    { _gpsrsPipelineName = Nothing
    , _gpsrsCreated = Nothing
    , _gpsrsStageStates = Nothing
    , _gpsrsPipelineVersion = Nothing
    , _gpsrsUpdated = Nothing
    , _gpsrsResponseStatus = pResponseStatus_
    }


-- | The name of the pipeline for which you want to get the state.
gpsrsPipelineName :: Lens' GetPipelineStateResponse (Maybe Text)
gpsrsPipelineName = lens _gpsrsPipelineName (\ s a -> s{_gpsrsPipelineName = a})

-- | The date and time the pipeline was created, in timestamp format.
gpsrsCreated :: Lens' GetPipelineStateResponse (Maybe UTCTime)
gpsrsCreated = lens _gpsrsCreated (\ s a -> s{_gpsrsCreated = a}) . mapping _Time

-- | A list of the pipeline stage output information, including stage name, state, most recent run details, whether the stage is disabled, and other data.
gpsrsStageStates :: Lens' GetPipelineStateResponse [StageState]
gpsrsStageStates = lens _gpsrsStageStates (\ s a -> s{_gpsrsStageStates = a}) . _Default . _Coerce

-- | The version number of the pipeline.
gpsrsPipelineVersion :: Lens' GetPipelineStateResponse (Maybe Natural)
gpsrsPipelineVersion = lens _gpsrsPipelineVersion (\ s a -> s{_gpsrsPipelineVersion = a}) . mapping _Nat

-- | The date and time the pipeline was last updated, in timestamp format.
gpsrsUpdated :: Lens' GetPipelineStateResponse (Maybe UTCTime)
gpsrsUpdated = lens _gpsrsUpdated (\ s a -> s{_gpsrsUpdated = a}) . mapping _Time

-- | -- | The response status code.
gpsrsResponseStatus :: Lens' GetPipelineStateResponse Int
gpsrsResponseStatus = lens _gpsrsResponseStatus (\ s a -> s{_gpsrsResponseStatus = a})

instance NFData GetPipelineStateResponse where
