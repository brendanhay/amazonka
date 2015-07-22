{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.GetPipelineDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the definition of the specified pipeline. You can call
-- @GetPipelineDefinition@ to retrieve the pipeline definition that you
-- provided using PutPipelineDefinition.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_GetPipelineDefinition.html>
module Network.AWS.DataPipeline.GetPipelineDefinition
    (
    -- * Request
      GetPipelineDefinition
    -- ** Request constructor
    , getPipelineDefinition
    -- ** Request lenses
    , gpdrqVersion
    , gpdrqPipelineId

    -- * Response
    , GetPipelineDefinitionResponse
    -- ** Response constructor
    , getPipelineDefinitionResponse
    -- ** Response lenses
    , gpdrsPipelineObjects
    , gpdrsParameterObjects
    , gpdrsParameterValues
    , gpdrsStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for GetPipelineDefinition.
--
-- /See:/ 'getPipelineDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdrqVersion'
--
-- * 'gpdrqPipelineId'
data GetPipelineDefinition = GetPipelineDefinition'
    { _gpdrqVersion    :: !(Maybe Text)
    , _gpdrqPipelineId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPipelineDefinition' smart constructor.
getPipelineDefinition :: Text -> GetPipelineDefinition
getPipelineDefinition pPipelineId =
    GetPipelineDefinition'
    { _gpdrqVersion = Nothing
    , _gpdrqPipelineId = pPipelineId
    }

-- | The version of the pipeline definition to retrieve. Set this parameter
-- to @latest@ (default) to use the last definition saved to the pipeline
-- or @active@ to use the last definition that was activated.
gpdrqVersion :: Lens' GetPipelineDefinition (Maybe Text)
gpdrqVersion = lens _gpdrqVersion (\ s a -> s{_gpdrqVersion = a});

-- | The ID of the pipeline.
gpdrqPipelineId :: Lens' GetPipelineDefinition Text
gpdrqPipelineId = lens _gpdrqPipelineId (\ s a -> s{_gpdrqPipelineId = a});

instance AWSRequest GetPipelineDefinition where
        type Sv GetPipelineDefinition = DataPipeline
        type Rs GetPipelineDefinition =
             GetPipelineDefinitionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetPipelineDefinitionResponse' <$>
                   (x .?> "pipelineObjects" .!@ mempty) <*>
                     (x .?> "parameterObjects" .!@ mempty)
                     <*> (x .?> "parameterValues" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders GetPipelineDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.GetPipelineDefinition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPipelineDefinition where
        toJSON GetPipelineDefinition'{..}
          = object
              ["version" .= _gpdrqVersion,
               "pipelineId" .= _gpdrqPipelineId]

instance ToPath GetPipelineDefinition where
        toPath = const "/"

instance ToQuery GetPipelineDefinition where
        toQuery = const mempty

-- | Contains the output of GetPipelineDefinition.
--
-- /See:/ 'getPipelineDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdrsPipelineObjects'
--
-- * 'gpdrsParameterObjects'
--
-- * 'gpdrsParameterValues'
--
-- * 'gpdrsStatus'
data GetPipelineDefinitionResponse = GetPipelineDefinitionResponse'
    { _gpdrsPipelineObjects  :: !(Maybe [PipelineObject])
    , _gpdrsParameterObjects :: !(Maybe [ParameterObject])
    , _gpdrsParameterValues  :: !(Maybe [ParameterValue])
    , _gpdrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetPipelineDefinitionResponse' smart constructor.
getPipelineDefinitionResponse :: Int -> GetPipelineDefinitionResponse
getPipelineDefinitionResponse pStatus =
    GetPipelineDefinitionResponse'
    { _gpdrsPipelineObjects = Nothing
    , _gpdrsParameterObjects = Nothing
    , _gpdrsParameterValues = Nothing
    , _gpdrsStatus = pStatus
    }

-- | The objects defined in the pipeline.
gpdrsPipelineObjects :: Lens' GetPipelineDefinitionResponse [PipelineObject]
gpdrsPipelineObjects = lens _gpdrsPipelineObjects (\ s a -> s{_gpdrsPipelineObjects = a}) . _Default;

-- | The parameter objects used in the pipeline definition.
gpdrsParameterObjects :: Lens' GetPipelineDefinitionResponse [ParameterObject]
gpdrsParameterObjects = lens _gpdrsParameterObjects (\ s a -> s{_gpdrsParameterObjects = a}) . _Default;

-- | The parameter values used in the pipeline definition.
gpdrsParameterValues :: Lens' GetPipelineDefinitionResponse [ParameterValue]
gpdrsParameterValues = lens _gpdrsParameterValues (\ s a -> s{_gpdrsParameterValues = a}) . _Default;

-- | FIXME: Undocumented member.
gpdrsStatus :: Lens' GetPipelineDefinitionResponse Int
gpdrsStatus = lens _gpdrsStatus (\ s a -> s{_gpdrsStatus = a});
