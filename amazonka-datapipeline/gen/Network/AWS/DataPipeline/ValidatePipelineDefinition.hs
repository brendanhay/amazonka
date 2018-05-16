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
-- Module      : Network.AWS.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline definition to ensure that it is well formed and can be run without error.
--
--
module Network.AWS.DataPipeline.ValidatePipelineDefinition
    (
    -- * Creating a Request
      validatePipelineDefinition
    , ValidatePipelineDefinition
    -- * Request Lenses
    , vpdParameterObjects
    , vpdParameterValues
    , vpdPipelineId
    , vpdPipelineObjects

    -- * Destructuring the Response
    , validatePipelineDefinitionResponse
    , ValidatePipelineDefinitionResponse
    -- * Response Lenses
    , vpdrsValidationErrors
    , vpdrsValidationWarnings
    , vpdrsResponseStatus
    , vpdrsErrored
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ValidatePipelineDefinition.
--
--
--
-- /See:/ 'validatePipelineDefinition' smart constructor.
data ValidatePipelineDefinition = ValidatePipelineDefinition'
  { _vpdParameterObjects :: !(Maybe [ParameterObject])
  , _vpdParameterValues  :: !(Maybe [ParameterValue])
  , _vpdPipelineId       :: !Text
  , _vpdPipelineObjects  :: ![PipelineObject]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidatePipelineDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpdParameterObjects' - The parameter objects used with the pipeline.
--
-- * 'vpdParameterValues' - The parameter values used with the pipeline.
--
-- * 'vpdPipelineId' - The ID of the pipeline.
--
-- * 'vpdPipelineObjects' - The objects that define the pipeline changes to validate against the pipeline.
validatePipelineDefinition
    :: Text -- ^ 'vpdPipelineId'
    -> ValidatePipelineDefinition
validatePipelineDefinition pPipelineId_ =
  ValidatePipelineDefinition'
    { _vpdParameterObjects = Nothing
    , _vpdParameterValues = Nothing
    , _vpdPipelineId = pPipelineId_
    , _vpdPipelineObjects = mempty
    }


-- | The parameter objects used with the pipeline.
vpdParameterObjects :: Lens' ValidatePipelineDefinition [ParameterObject]
vpdParameterObjects = lens _vpdParameterObjects (\ s a -> s{_vpdParameterObjects = a}) . _Default . _Coerce

-- | The parameter values used with the pipeline.
vpdParameterValues :: Lens' ValidatePipelineDefinition [ParameterValue]
vpdParameterValues = lens _vpdParameterValues (\ s a -> s{_vpdParameterValues = a}) . _Default . _Coerce

-- | The ID of the pipeline.
vpdPipelineId :: Lens' ValidatePipelineDefinition Text
vpdPipelineId = lens _vpdPipelineId (\ s a -> s{_vpdPipelineId = a})

-- | The objects that define the pipeline changes to validate against the pipeline.
vpdPipelineObjects :: Lens' ValidatePipelineDefinition [PipelineObject]
vpdPipelineObjects = lens _vpdPipelineObjects (\ s a -> s{_vpdPipelineObjects = a}) . _Coerce

instance AWSRequest ValidatePipelineDefinition where
        type Rs ValidatePipelineDefinition =
             ValidatePipelineDefinitionResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 ValidatePipelineDefinitionResponse' <$>
                   (x .?> "validationErrors" .!@ mempty) <*>
                     (x .?> "validationWarnings" .!@ mempty)
                     <*> (pure (fromEnum s))
                     <*> (x .:> "errored"))

instance Hashable ValidatePipelineDefinition where

instance NFData ValidatePipelineDefinition where

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
              (catMaybes
                 [("parameterObjects" .=) <$> _vpdParameterObjects,
                  ("parameterValues" .=) <$> _vpdParameterValues,
                  Just ("pipelineId" .= _vpdPipelineId),
                  Just ("pipelineObjects" .= _vpdPipelineObjects)])

instance ToPath ValidatePipelineDefinition where
        toPath = const "/"

instance ToQuery ValidatePipelineDefinition where
        toQuery = const mempty

-- | Contains the output of ValidatePipelineDefinition.
--
--
--
-- /See:/ 'validatePipelineDefinitionResponse' smart constructor.
data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse'
  { _vpdrsValidationErrors   :: !(Maybe [ValidationError])
  , _vpdrsValidationWarnings :: !(Maybe [ValidationWarning])
  , _vpdrsResponseStatus     :: !Int
  , _vpdrsErrored            :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidatePipelineDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpdrsValidationErrors' - Any validation errors that were found.
--
-- * 'vpdrsValidationWarnings' - Any validation warnings that were found.
--
-- * 'vpdrsResponseStatus' - -- | The response status code.
--
-- * 'vpdrsErrored' - Indicates whether there were validation errors.
validatePipelineDefinitionResponse
    :: Int -- ^ 'vpdrsResponseStatus'
    -> Bool -- ^ 'vpdrsErrored'
    -> ValidatePipelineDefinitionResponse
validatePipelineDefinitionResponse pResponseStatus_ pErrored_ =
  ValidatePipelineDefinitionResponse'
    { _vpdrsValidationErrors = Nothing
    , _vpdrsValidationWarnings = Nothing
    , _vpdrsResponseStatus = pResponseStatus_
    , _vpdrsErrored = pErrored_
    }


-- | Any validation errors that were found.
vpdrsValidationErrors :: Lens' ValidatePipelineDefinitionResponse [ValidationError]
vpdrsValidationErrors = lens _vpdrsValidationErrors (\ s a -> s{_vpdrsValidationErrors = a}) . _Default . _Coerce

-- | Any validation warnings that were found.
vpdrsValidationWarnings :: Lens' ValidatePipelineDefinitionResponse [ValidationWarning]
vpdrsValidationWarnings = lens _vpdrsValidationWarnings (\ s a -> s{_vpdrsValidationWarnings = a}) . _Default . _Coerce

-- | -- | The response status code.
vpdrsResponseStatus :: Lens' ValidatePipelineDefinitionResponse Int
vpdrsResponseStatus = lens _vpdrsResponseStatus (\ s a -> s{_vpdrsResponseStatus = a})

-- | Indicates whether there were validation errors.
vpdrsErrored :: Lens' ValidatePipelineDefinitionResponse Bool
vpdrsErrored = lens _vpdrsErrored (\ s a -> s{_vpdrsErrored = a})

instance NFData ValidatePipelineDefinitionResponse
         where
