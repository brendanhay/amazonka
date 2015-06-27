{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Validates the specified pipeline definition to ensure that it is well
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
    , vpdParameterObjects
    , vpdParameterValues
    , vpdPipelineId
    , vpdPipelineObjects

    -- * Response
    , ValidatePipelineDefinitionResponse
    -- ** Response constructor
    , validatePipelineDefinitionResponse
    -- ** Response lenses
    , vpdrValidationErrors
    , vpdrValidationWarnings
    , vpdrErrored
    , vpdrStatus
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
-- * 'vpdParameterObjects'
--
-- * 'vpdParameterValues'
--
-- * 'vpdPipelineId'
--
-- * 'vpdPipelineObjects'
data ValidatePipelineDefinition = ValidatePipelineDefinition'
    { _vpdParameterObjects :: !(Maybe [ParameterObject])
    , _vpdParameterValues  :: !(Maybe [ParameterValue])
    , _vpdPipelineId       :: !Text
    , _vpdPipelineObjects  :: ![PipelineObject]
    } deriving (Eq,Read,Show)

-- | 'ValidatePipelineDefinition' smart constructor.
validatePipelineDefinition :: Text -> ValidatePipelineDefinition
validatePipelineDefinition pPipelineId =
    ValidatePipelineDefinition'
    { _vpdParameterObjects = Nothing
    , _vpdParameterValues = Nothing
    , _vpdPipelineId = pPipelineId
    , _vpdPipelineObjects = mempty
    }

-- | The parameter objects used with the pipeline.
vpdParameterObjects :: Lens' ValidatePipelineDefinition [ParameterObject]
vpdParameterObjects = lens _vpdParameterObjects (\ s a -> s{_vpdParameterObjects = a}) . _Default;

-- | The parameter values used with the pipeline.
vpdParameterValues :: Lens' ValidatePipelineDefinition [ParameterValue]
vpdParameterValues = lens _vpdParameterValues (\ s a -> s{_vpdParameterValues = a}) . _Default;

-- | The ID of the pipeline.
vpdPipelineId :: Lens' ValidatePipelineDefinition Text
vpdPipelineId = lens _vpdPipelineId (\ s a -> s{_vpdPipelineId = a});

-- | The objects that define the pipeline changes to validate against the
-- pipeline.
vpdPipelineObjects :: Lens' ValidatePipelineDefinition [PipelineObject]
vpdPipelineObjects = lens _vpdPipelineObjects (\ s a -> s{_vpdPipelineObjects = a});

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
                     <*> (x .:> "errored")
                     <*> (pure (fromEnum s)))

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
              ["parameterObjects" .= _vpdParameterObjects,
               "parameterValues" .= _vpdParameterValues,
               "pipelineId" .= _vpdPipelineId,
               "pipelineObjects" .= _vpdPipelineObjects]

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
-- * 'vpdrValidationErrors'
--
-- * 'vpdrValidationWarnings'
--
-- * 'vpdrErrored'
--
-- * 'vpdrStatus'
data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse'
    { _vpdrValidationErrors   :: !(Maybe [ValidationError])
    , _vpdrValidationWarnings :: !(Maybe [ValidationWarning])
    , _vpdrErrored            :: !Bool
    , _vpdrStatus             :: !Int
    } deriving (Eq,Read,Show)

-- | 'ValidatePipelineDefinitionResponse' smart constructor.
validatePipelineDefinitionResponse :: Bool -> Int -> ValidatePipelineDefinitionResponse
validatePipelineDefinitionResponse pErrored pStatus =
    ValidatePipelineDefinitionResponse'
    { _vpdrValidationErrors = Nothing
    , _vpdrValidationWarnings = Nothing
    , _vpdrErrored = pErrored
    , _vpdrStatus = pStatus
    }

-- | Any validation errors that were found.
vpdrValidationErrors :: Lens' ValidatePipelineDefinitionResponse [ValidationError]
vpdrValidationErrors = lens _vpdrValidationErrors (\ s a -> s{_vpdrValidationErrors = a}) . _Default;

-- | Any validation warnings that were found.
vpdrValidationWarnings :: Lens' ValidatePipelineDefinitionResponse [ValidationWarning]
vpdrValidationWarnings = lens _vpdrValidationWarnings (\ s a -> s{_vpdrValidationWarnings = a}) . _Default;

-- | Indicates whether there were validation errors.
vpdrErrored :: Lens' ValidatePipelineDefinitionResponse Bool
vpdrErrored = lens _vpdrErrored (\ s a -> s{_vpdrErrored = a});

-- | FIXME: Undocumented member.
vpdrStatus :: Lens' ValidatePipelineDefinitionResponse Int
vpdrStatus = lens _vpdrStatus (\ s a -> s{_vpdrStatus = a});
