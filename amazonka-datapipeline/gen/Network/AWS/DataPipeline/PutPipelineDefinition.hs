{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.PutPipelineDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds tasks, schedules, and preconditions to the specified pipeline. You
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
    , ppdrqParameterObjects
    , ppdrqParameterValues
    , ppdrqPipelineId
    , ppdrqPipelineObjects

    -- * Response
    , PutPipelineDefinitionResponse
    -- ** Response constructor
    , putPipelineDefinitionResponse
    -- ** Response lenses
    , ppdrsValidationErrors
    , ppdrsValidationWarnings
    , ppdrsStatus
    , ppdrsErrored
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
-- * 'ppdrqParameterObjects'
--
-- * 'ppdrqParameterValues'
--
-- * 'ppdrqPipelineId'
--
-- * 'ppdrqPipelineObjects'
data PutPipelineDefinition = PutPipelineDefinition'
    { _ppdrqParameterObjects :: !(Maybe [ParameterObject])
    , _ppdrqParameterValues  :: !(Maybe [ParameterValue])
    , _ppdrqPipelineId       :: !Text
    , _ppdrqPipelineObjects  :: ![PipelineObject]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutPipelineDefinition' smart constructor.
putPipelineDefinition :: Text -> PutPipelineDefinition
putPipelineDefinition pPipelineId_ =
    PutPipelineDefinition'
    { _ppdrqParameterObjects = Nothing
    , _ppdrqParameterValues = Nothing
    , _ppdrqPipelineId = pPipelineId_
    , _ppdrqPipelineObjects = mempty
    }

-- | The parameter objects used with the pipeline.
ppdrqParameterObjects :: Lens' PutPipelineDefinition [ParameterObject]
ppdrqParameterObjects = lens _ppdrqParameterObjects (\ s a -> s{_ppdrqParameterObjects = a}) . _Default;

-- | The parameter values used with the pipeline.
ppdrqParameterValues :: Lens' PutPipelineDefinition [ParameterValue]
ppdrqParameterValues = lens _ppdrqParameterValues (\ s a -> s{_ppdrqParameterValues = a}) . _Default;

-- | The ID of the pipeline.
ppdrqPipelineId :: Lens' PutPipelineDefinition Text
ppdrqPipelineId = lens _ppdrqPipelineId (\ s a -> s{_ppdrqPipelineId = a});

-- | The objects that define the pipeline. These objects overwrite the
-- existing pipeline definition.
ppdrqPipelineObjects :: Lens' PutPipelineDefinition [PipelineObject]
ppdrqPipelineObjects = lens _ppdrqPipelineObjects (\ s a -> s{_ppdrqPipelineObjects = a});

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
              ["parameterObjects" .= _ppdrqParameterObjects,
               "parameterValues" .= _ppdrqParameterValues,
               "pipelineId" .= _ppdrqPipelineId,
               "pipelineObjects" .= _ppdrqPipelineObjects]

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
-- * 'ppdrsValidationErrors'
--
-- * 'ppdrsValidationWarnings'
--
-- * 'ppdrsStatus'
--
-- * 'ppdrsErrored'
data PutPipelineDefinitionResponse = PutPipelineDefinitionResponse'
    { _ppdrsValidationErrors   :: !(Maybe [ValidationError])
    , _ppdrsValidationWarnings :: !(Maybe [ValidationWarning])
    , _ppdrsStatus             :: !Int
    , _ppdrsErrored            :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutPipelineDefinitionResponse' smart constructor.
putPipelineDefinitionResponse :: Int -> Bool -> PutPipelineDefinitionResponse
putPipelineDefinitionResponse pStatus_ pErrored_ =
    PutPipelineDefinitionResponse'
    { _ppdrsValidationErrors = Nothing
    , _ppdrsValidationWarnings = Nothing
    , _ppdrsStatus = pStatus_
    , _ppdrsErrored = pErrored_
    }

-- | The validation errors that are associated with the objects defined in
-- @pipelineObjects@.
ppdrsValidationErrors :: Lens' PutPipelineDefinitionResponse [ValidationError]
ppdrsValidationErrors = lens _ppdrsValidationErrors (\ s a -> s{_ppdrsValidationErrors = a}) . _Default;

-- | The validation warnings that are associated with the objects defined in
-- @pipelineObjects@.
ppdrsValidationWarnings :: Lens' PutPipelineDefinitionResponse [ValidationWarning]
ppdrsValidationWarnings = lens _ppdrsValidationWarnings (\ s a -> s{_ppdrsValidationWarnings = a}) . _Default;

-- | FIXME: Undocumented member.
ppdrsStatus :: Lens' PutPipelineDefinitionResponse Int
ppdrsStatus = lens _ppdrsStatus (\ s a -> s{_ppdrsStatus = a});

-- | Indicates whether there were validation errors, and the pipeline
-- definition is stored but cannot be activated until you correct the
-- pipeline and call @PutPipelineDefinition@ to commit the corrected
-- pipeline.
ppdrsErrored :: Lens' PutPipelineDefinitionResponse Bool
ppdrsErrored = lens _ppdrsErrored (\ s a -> s{_ppdrsErrored = a});
