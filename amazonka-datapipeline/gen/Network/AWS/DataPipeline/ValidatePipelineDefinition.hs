{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline definition to ensure that it is well
-- formed and can be run without error.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_ValidatePipelineDefinition.html>
module Network.AWS.DataPipeline.ValidatePipelineDefinition
    (
    -- * Request
      ValidatePipelineDefinition
    -- ** Request constructor
    , validatePipelineDefinition
    -- ** Request lenses
    , vpdrqParameterObjects
    , vpdrqParameterValues
    , vpdrqPipelineId
    , vpdrqPipelineObjects

    -- * Response
    , ValidatePipelineDefinitionResponse
    -- ** Response constructor
    , validatePipelineDefinitionResponse
    -- ** Response lenses
    , vpdrsValidationErrors
    , vpdrsValidationWarnings
    , vpdrsStatus
    , vpdrsErrored
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for ValidatePipelineDefinition.
--
-- /See:/ 'validatePipelineDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpdrqParameterObjects'
--
-- * 'vpdrqParameterValues'
--
-- * 'vpdrqPipelineId'
--
-- * 'vpdrqPipelineObjects'
data ValidatePipelineDefinition = ValidatePipelineDefinition'
    { _vpdrqParameterObjects :: !(Maybe [ParameterObject])
    , _vpdrqParameterValues  :: !(Maybe [ParameterValue])
    , _vpdrqPipelineId       :: !Text
    , _vpdrqPipelineObjects  :: ![PipelineObject]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ValidatePipelineDefinition' smart constructor.
validatePipelineDefinition :: Text -> ValidatePipelineDefinition
validatePipelineDefinition pPipelineId =
    ValidatePipelineDefinition'
    { _vpdrqParameterObjects = Nothing
    , _vpdrqParameterValues = Nothing
    , _vpdrqPipelineId = pPipelineId
    , _vpdrqPipelineObjects = mempty
    }

-- | The parameter objects used with the pipeline.
vpdrqParameterObjects :: Lens' ValidatePipelineDefinition [ParameterObject]
vpdrqParameterObjects = lens _vpdrqParameterObjects (\ s a -> s{_vpdrqParameterObjects = a}) . _Default;

-- | The parameter values used with the pipeline.
vpdrqParameterValues :: Lens' ValidatePipelineDefinition [ParameterValue]
vpdrqParameterValues = lens _vpdrqParameterValues (\ s a -> s{_vpdrqParameterValues = a}) . _Default;

-- | The ID of the pipeline.
vpdrqPipelineId :: Lens' ValidatePipelineDefinition Text
vpdrqPipelineId = lens _vpdrqPipelineId (\ s a -> s{_vpdrqPipelineId = a});

-- | The objects that define the pipeline changes to validate against the
-- pipeline.
vpdrqPipelineObjects :: Lens' ValidatePipelineDefinition [PipelineObject]
vpdrqPipelineObjects = lens _vpdrqPipelineObjects (\ s a -> s{_vpdrqPipelineObjects = a});

instance AWSRequest ValidatePipelineDefinition where
        type Sv ValidatePipelineDefinition = DataPipeline
        type Rs ValidatePipelineDefinition =
             ValidatePipelineDefinitionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ValidatePipelineDefinitionResponse' <$>
                   (x .?> "validationErrors" .!@ mempty) <*>
                     (x .?> "validationWarnings" .!@ mempty)
                     <*> (pure (fromEnum s))
                     <*> (x .:> "errored"))

instance ToHeaders ValidatePipelineDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.ValidatePipelineDefinition" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ValidatePipelineDefinition where
        toJSON ValidatePipelineDefinition'{..}
          = object
              ["parameterObjects" .= _vpdrqParameterObjects,
               "parameterValues" .= _vpdrqParameterValues,
               "pipelineId" .= _vpdrqPipelineId,
               "pipelineObjects" .= _vpdrqPipelineObjects]

instance ToPath ValidatePipelineDefinition where
        toPath = const "/"

instance ToQuery ValidatePipelineDefinition where
        toQuery = const mempty

-- | Contains the output of ValidatePipelineDefinition.
--
-- /See:/ 'validatePipelineDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vpdrsValidationErrors'
--
-- * 'vpdrsValidationWarnings'
--
-- * 'vpdrsStatus'
--
-- * 'vpdrsErrored'
data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse'
    { _vpdrsValidationErrors   :: !(Maybe [ValidationError])
    , _vpdrsValidationWarnings :: !(Maybe [ValidationWarning])
    , _vpdrsStatus             :: !Int
    , _vpdrsErrored            :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ValidatePipelineDefinitionResponse' smart constructor.
validatePipelineDefinitionResponse :: Int -> Bool -> ValidatePipelineDefinitionResponse
validatePipelineDefinitionResponse pStatus pErrored =
    ValidatePipelineDefinitionResponse'
    { _vpdrsValidationErrors = Nothing
    , _vpdrsValidationWarnings = Nothing
    , _vpdrsStatus = pStatus
    , _vpdrsErrored = pErrored
    }

-- | Any validation errors that were found.
vpdrsValidationErrors :: Lens' ValidatePipelineDefinitionResponse [ValidationError]
vpdrsValidationErrors = lens _vpdrsValidationErrors (\ s a -> s{_vpdrsValidationErrors = a}) . _Default;

-- | Any validation warnings that were found.
vpdrsValidationWarnings :: Lens' ValidatePipelineDefinitionResponse [ValidationWarning]
vpdrsValidationWarnings = lens _vpdrsValidationWarnings (\ s a -> s{_vpdrsValidationWarnings = a}) . _Default;

-- | FIXME: Undocumented member.
vpdrsStatus :: Lens' ValidatePipelineDefinitionResponse Int
vpdrsStatus = lens _vpdrsStatus (\ s a -> s{_vpdrsStatus = a});

-- | Indicates whether there were validation errors.
vpdrsErrored :: Lens' ValidatePipelineDefinitionResponse Bool
vpdrsErrored = lens _vpdrsErrored (\ s a -> s{_vpdrsErrored = a});
