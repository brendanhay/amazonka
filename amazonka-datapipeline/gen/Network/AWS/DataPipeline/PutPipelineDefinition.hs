{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DataPipeline.PutPipelineDefinition
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

-- | Adds tasks, schedules, and preconditions to the specified pipeline. You
-- can use @PutPipelineDefinition@ to populate a new pipeline.
--
-- @PutPipelineDefinition@ also validates the configuration as it adds it
-- to the pipeline. Changes to the pipeline are saved unless one of the
-- following three validation errors exists in the pipeline.
--
-- 1.  An object is missing a name or identifier field.
-- 2.  A string or reference field is empty.
-- 3.  The number of objects in the pipeline exceeds the maximum allowed
--     objects.
-- 4.  The pipeline is in a FINISHED state.
--
-- Pipeline object definitions are passed to the @PutPipelineDefinition@
-- action and returned by the GetPipelineDefinition action.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_PutPipelineDefinition.html>
module Network.AWS.DataPipeline.PutPipelineDefinition
    (
    -- * Request
      PutPipelineDefinition
    -- ** Request constructor
    , putPipelineDefinition
    -- ** Request lenses
    , ppdParameterObjects
    , ppdParameterValues
    , ppdPipelineId
    , ppdPipelineObjects

    -- * Response
    , PutPipelineDefinitionResponse
    -- ** Response constructor
    , putPipelineDefinitionResponse
    -- ** Response lenses
    , ppdrValidationErrors
    , ppdrValidationWarnings
    , ppdrErrored
    , ppdrStatus
    ) where

import           Network.AWS.DataPipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for PutPipelineDefinition.
--
-- /See:/ 'putPipelineDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ppdParameterObjects'
--
-- * 'ppdParameterValues'
--
-- * 'ppdPipelineId'
--
-- * 'ppdPipelineObjects'
data PutPipelineDefinition = PutPipelineDefinition'
    { _ppdParameterObjects :: !(Maybe [ParameterObject])
    , _ppdParameterValues  :: !(Maybe [ParameterValue])
    , _ppdPipelineId       :: !Text
    , _ppdPipelineObjects  :: ![PipelineObject]
    } deriving (Eq,Read,Show)

-- | 'PutPipelineDefinition' smart constructor.
putPipelineDefinition :: Text -> PutPipelineDefinition
putPipelineDefinition pPipelineId =
    PutPipelineDefinition'
    { _ppdParameterObjects = Nothing
    , _ppdParameterValues = Nothing
    , _ppdPipelineId = pPipelineId
    , _ppdPipelineObjects = mempty
    }

-- | The parameter objects used with the pipeline.
ppdParameterObjects :: Lens' PutPipelineDefinition [ParameterObject]
ppdParameterObjects = lens _ppdParameterObjects (\ s a -> s{_ppdParameterObjects = a}) . _Default;

-- | The parameter values used with the pipeline.
ppdParameterValues :: Lens' PutPipelineDefinition [ParameterValue]
ppdParameterValues = lens _ppdParameterValues (\ s a -> s{_ppdParameterValues = a}) . _Default;

-- | The ID of the pipeline.
ppdPipelineId :: Lens' PutPipelineDefinition Text
ppdPipelineId = lens _ppdPipelineId (\ s a -> s{_ppdPipelineId = a});

-- | The objects that define the pipeline. These objects overwrite the
-- existing pipeline definition.
ppdPipelineObjects :: Lens' PutPipelineDefinition [PipelineObject]
ppdPipelineObjects = lens _ppdPipelineObjects (\ s a -> s{_ppdPipelineObjects = a});

instance AWSRequest PutPipelineDefinition where
        type Sv PutPipelineDefinition = DataPipeline
        type Rs PutPipelineDefinition =
             PutPipelineDefinitionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 PutPipelineDefinitionResponse' <$>
                   (x .?> "validationErrors" .!@ mempty) <*>
                     (x .?> "validationWarnings" .!@ mempty)
                     <*> (x .:> "errored")
                     <*> (pure (fromEnum s)))

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
              ["parameterObjects" .= _ppdParameterObjects,
               "parameterValues" .= _ppdParameterValues,
               "pipelineId" .= _ppdPipelineId,
               "pipelineObjects" .= _ppdPipelineObjects]

instance ToPath PutPipelineDefinition where
        toPath = const "/"

instance ToQuery PutPipelineDefinition where
        toQuery = const mempty

-- | Contains the output of PutPipelineDefinition.
--
-- /See:/ 'putPipelineDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ppdrValidationErrors'
--
-- * 'ppdrValidationWarnings'
--
-- * 'ppdrErrored'
--
-- * 'ppdrStatus'
data PutPipelineDefinitionResponse = PutPipelineDefinitionResponse'
    { _ppdrValidationErrors   :: !(Maybe [ValidationError])
    , _ppdrValidationWarnings :: !(Maybe [ValidationWarning])
    , _ppdrErrored            :: !Bool
    , _ppdrStatus             :: !Int
    } deriving (Eq,Read,Show)

-- | 'PutPipelineDefinitionResponse' smart constructor.
putPipelineDefinitionResponse :: Bool -> Int -> PutPipelineDefinitionResponse
putPipelineDefinitionResponse pErrored pStatus =
    PutPipelineDefinitionResponse'
    { _ppdrValidationErrors = Nothing
    , _ppdrValidationWarnings = Nothing
    , _ppdrErrored = pErrored
    , _ppdrStatus = pStatus
    }

-- | The validation errors that are associated with the objects defined in
-- @pipelineObjects@.
ppdrValidationErrors :: Lens' PutPipelineDefinitionResponse [ValidationError]
ppdrValidationErrors = lens _ppdrValidationErrors (\ s a -> s{_ppdrValidationErrors = a}) . _Default;

-- | The validation warnings that are associated with the objects defined in
-- @pipelineObjects@.
ppdrValidationWarnings :: Lens' PutPipelineDefinitionResponse [ValidationWarning]
ppdrValidationWarnings = lens _ppdrValidationWarnings (\ s a -> s{_ppdrValidationWarnings = a}) . _Default;

-- | Indicates whether there were validation errors, and the pipeline
-- definition is stored but cannot be activated until you correct the
-- pipeline and call @PutPipelineDefinition@ to commit the corrected
-- pipeline.
ppdrErrored :: Lens' PutPipelineDefinitionResponse Bool
ppdrErrored = lens _ppdrErrored (\ s a -> s{_ppdrErrored = a});

-- | FIXME: Undocumented member.
ppdrStatus :: Lens' PutPipelineDefinitionResponse Int
ppdrStatus = lens _ppdrStatus (\ s a -> s{_ppdrStatus = a});
