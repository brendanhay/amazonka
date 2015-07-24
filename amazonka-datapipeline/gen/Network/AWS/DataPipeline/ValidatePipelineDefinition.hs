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
    , vpdParameterObjects
    , vpdParameterValues
    , vpdPipelineId
    , vpdPipelineObjects

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ValidatePipelineDefinition' smart constructor.
validatePipelineDefinition :: Text -> ValidatePipelineDefinition
validatePipelineDefinition pPipelineId_ =
    ValidatePipelineDefinition'
    { _vpdParameterObjects = Nothing
    , _vpdParameterValues = Nothing
    , _vpdPipelineId = pPipelineId_
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
        request = postJSON "ValidatePipelineDefinition"
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
validatePipelineDefinitionResponse pStatus_ pErrored_ =
    ValidatePipelineDefinitionResponse'
    { _vpdrsValidationErrors = Nothing
    , _vpdrsValidationWarnings = Nothing
    , _vpdrsStatus = pStatus_
    , _vpdrsErrored = pErrored_
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
