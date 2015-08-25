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
-- Module      : Network.AWS.DataPipeline.PutPipelineDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tasks, schedules, and preconditions to the specified pipeline. You
-- can use 'PutPipelineDefinition' to populate a new pipeline.
--
-- 'PutPipelineDefinition' also validates the configuration as it adds it
-- to the pipeline. Changes to the pipeline are saved unless one of the
-- following three validation errors exists in the pipeline.
--
-- 1.  An object is missing a name or identifier field.
-- 2.  A string or reference field is empty.
-- 3.  The number of objects in the pipeline exceeds the maximum allowed
--     objects.
-- 4.  The pipeline is in a FINISHED state.
--
-- Pipeline object definitions are passed to the 'PutPipelineDefinition'
-- action and returned by the GetPipelineDefinition action.
--
-- /See:/ <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_PutPipelineDefinition.html AWS API Reference> for PutPipelineDefinition.
module Network.AWS.DataPipeline.PutPipelineDefinition
    (
    -- * Creating a Request
      putPipelineDefinition
    , PutPipelineDefinition
    -- * Request Lenses
    , ppdParameterObjects
    , ppdParameterValues
    , ppdPipelineId
    , ppdPipelineObjects

    -- * Destructuring the Response
    , putPipelineDefinitionResponse
    , PutPipelineDefinitionResponse
    -- * Response Lenses
    , ppdrsValidationErrors
    , ppdrsValidationWarnings
    , ppdrsStatus
    , ppdrsErrored
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.DataPipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for PutPipelineDefinition.
--
-- /See:/ 'putPipelineDefinition' smart constructor.
data PutPipelineDefinition = PutPipelineDefinition'
    { _ppdParameterObjects :: !(Maybe [ParameterObject])
    , _ppdParameterValues  :: !(Maybe [ParameterValue])
    , _ppdPipelineId       :: !Text
    , _ppdPipelineObjects  :: ![PipelineObject]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutPipelineDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppdParameterObjects'
--
-- * 'ppdParameterValues'
--
-- * 'ppdPipelineId'
--
-- * 'ppdPipelineObjects'
putPipelineDefinition
    :: Text -- ^ 'ppdPipelineId'
    -> PutPipelineDefinition
putPipelineDefinition pPipelineId_ =
    PutPipelineDefinition'
    { _ppdParameterObjects = Nothing
    , _ppdParameterValues = Nothing
    , _ppdPipelineId = pPipelineId_
    , _ppdPipelineObjects = mempty
    }

-- | The parameter objects used with the pipeline.
ppdParameterObjects :: Lens' PutPipelineDefinition [ParameterObject]
ppdParameterObjects = lens _ppdParameterObjects (\ s a -> s{_ppdParameterObjects = a}) . _Default . _Coerce;

-- | The parameter values used with the pipeline.
ppdParameterValues :: Lens' PutPipelineDefinition [ParameterValue]
ppdParameterValues = lens _ppdParameterValues (\ s a -> s{_ppdParameterValues = a}) . _Default . _Coerce;

-- | The ID of the pipeline.
ppdPipelineId :: Lens' PutPipelineDefinition Text
ppdPipelineId = lens _ppdPipelineId (\ s a -> s{_ppdPipelineId = a});

-- | The objects that define the pipeline. These objects overwrite the
-- existing pipeline definition.
ppdPipelineObjects :: Lens' PutPipelineDefinition [PipelineObject]
ppdPipelineObjects = lens _ppdPipelineObjects (\ s a -> s{_ppdPipelineObjects = a}) . _Coerce;

instance AWSRequest PutPipelineDefinition where
        type Rs PutPipelineDefinition =
             PutPipelineDefinitionResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 PutPipelineDefinitionResponse' <$>
                   (x .?> "validationErrors" .!@ mempty) <*>
                     (x .?> "validationWarnings" .!@ mempty)
                     <*> (pure (fromEnum s))
                     <*> (x .:> "errored"))

instance ToHeaders PutPipelineDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.PutPipelineDefinition" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutPipelineDefinition where
        toJSON PutPipelineDefinition'{..}
          = object
              (catMaybes
                 [("parameterObjects" .=) <$> _ppdParameterObjects,
                  ("parameterValues" .=) <$> _ppdParameterValues,
                  Just ("pipelineId" .= _ppdPipelineId),
                  Just ("pipelineObjects" .= _ppdPipelineObjects)])

instance ToPath PutPipelineDefinition where
        toPath = const "/"

instance ToQuery PutPipelineDefinition where
        toQuery = const mempty

-- | Contains the output of PutPipelineDefinition.
--
-- /See:/ 'putPipelineDefinitionResponse' smart constructor.
data PutPipelineDefinitionResponse = PutPipelineDefinitionResponse'
    { _ppdrsValidationErrors   :: !(Maybe [ValidationError])
    , _ppdrsValidationWarnings :: !(Maybe [ValidationWarning])
    , _ppdrsStatus             :: !Int
    , _ppdrsErrored            :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutPipelineDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppdrsValidationErrors'
--
-- * 'ppdrsValidationWarnings'
--
-- * 'ppdrsStatus'
--
-- * 'ppdrsErrored'
putPipelineDefinitionResponse
    :: Int -- ^ 'ppdrsStatus'
    -> Bool -- ^ 'ppdrsErrored'
    -> PutPipelineDefinitionResponse
putPipelineDefinitionResponse pStatus_ pErrored_ =
    PutPipelineDefinitionResponse'
    { _ppdrsValidationErrors = Nothing
    , _ppdrsValidationWarnings = Nothing
    , _ppdrsStatus = pStatus_
    , _ppdrsErrored = pErrored_
    }

-- | The validation errors that are associated with the objects defined in
-- 'pipelineObjects'.
ppdrsValidationErrors :: Lens' PutPipelineDefinitionResponse [ValidationError]
ppdrsValidationErrors = lens _ppdrsValidationErrors (\ s a -> s{_ppdrsValidationErrors = a}) . _Default . _Coerce;

-- | The validation warnings that are associated with the objects defined in
-- 'pipelineObjects'.
ppdrsValidationWarnings :: Lens' PutPipelineDefinitionResponse [ValidationWarning]
ppdrsValidationWarnings = lens _ppdrsValidationWarnings (\ s a -> s{_ppdrsValidationWarnings = a}) . _Default . _Coerce;

-- | The response status code.
ppdrsStatus :: Lens' PutPipelineDefinitionResponse Int
ppdrsStatus = lens _ppdrsStatus (\ s a -> s{_ppdrsStatus = a});

-- | Indicates whether there were validation errors, and the pipeline
-- definition is stored but cannot be activated until you correct the
-- pipeline and call 'PutPipelineDefinition' to commit the corrected
-- pipeline.
ppdrsErrored :: Lens' PutPipelineDefinitionResponse Bool
ppdrsErrored = lens _ppdrsErrored (\ s a -> s{_ppdrsErrored = a});
