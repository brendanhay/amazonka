{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.ValidatePipelineDefinition
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Tests the pipeline definition with a set of validation checks to ensure
-- that it is well formed and can run without error. Example 1 This example
-- sets an valid pipeline configuration and returns success. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ValidatePipelineDefinition Content-Length: 936 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-06372391ZG65EXAMPLE",
-- "pipelineObjects": [ {"id": "Default", "name": "Default", "fields": [
-- {"key": "workerGroup", "stringValue": "MyworkerGroup"} ] }, {"id":
-- "Schedule", "name": "Schedule", "fields": [ {"key": "startDateTime",
-- "stringValue": "2012-09-25T17:00:00"}, {"key": "type", "stringValue":
-- "Schedule"}, {"key": "period", "stringValue": "1 hour"}, {"key":
-- "endDateTime", "stringValue": "2012-09-25T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } x-amzn-RequestId: 92c9f347-0776-11e2-8a14-21bb8a1f50ef
-- Content-Type: application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12
-- Nov 2012 17:50:53 GMT {"errored": false} Example 2 This example sets an
-- invalid pipeline configuration and returns the associated set of validation
-- errors. POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1
-- X-Amz-Target: DataPipeline.ValidatePipelineDefinition Content-Length: 903
-- Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012
-- 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE", "pipelineObjects": [ {"id": "Default", "name":
-- "Default", "fields": [ {"key": "workerGroup", "stringValue":
-- "MyworkerGroup"} ] }, {"id": "Schedule", "name": "Schedule", "fields": [
-- {"key": "startDateTime", "stringValue": "bad-time"}, {"key": "type",
-- "stringValue": "Schedule"}, {"key": "period", "stringValue": "1 hour"},
-- {"key": "endDateTime", "stringValue": "2012-09-25T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } x-amzn-RequestId: 496a1f5a-0e6a-11e2-a61c-bd6312c92ddd
-- Content-Type: application/x-amz-json-1.1 Content-Length: 278 Date: Mon, 12
-- Nov 2012 17:50:53 GMT {"errored": true, "validationErrors": [ {"errors":
-- ["INVALID_FIELD_VALUE: 'startDateTime' value must be a literal datetime
-- value."], "id": "Schedule"} ] }.
module Network.AWS.DataPipeline.ValidatePipelineDefinition
    (
    -- * Request
      ValidatePipelineDefinition
    -- ** Request constructor
    , validatePipelineDefinition
    -- ** Request lenses
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
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The input of the ValidatePipelineDefinition action.
data ValidatePipelineDefinition = ValidatePipelineDefinition
    { _vpdPipelineId :: Text
    , _vpdPipelineObjects :: [PipelineObject]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidatePipelineDefinition' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineId ::@ @Text@
--
-- * @PipelineObjects ::@ @[PipelineObject]@
--
validatePipelineDefinition :: Text -- ^ 'vpdPipelineId'
                           -> [PipelineObject] -- ^ 'vpdPipelineObjects'
                           -> ValidatePipelineDefinition
validatePipelineDefinition p1 p2 = ValidatePipelineDefinition
    { _vpdPipelineId = p1
    , _vpdPipelineObjects = p2
    }

-- | Identifies the pipeline whose definition is to be validated.
vpdPipelineId :: Lens' ValidatePipelineDefinition Text
vpdPipelineId = lens _vpdPipelineId (\s a -> s { _vpdPipelineId = a })

-- | A list of objects that define the pipeline changes to validate against the
-- pipeline.
vpdPipelineObjects :: Lens' ValidatePipelineDefinition [PipelineObject]
vpdPipelineObjects =
    lens _vpdPipelineObjects (\s a -> s { _vpdPipelineObjects = a })

instance ToPath ValidatePipelineDefinition

instance ToQuery ValidatePipelineDefinition

instance ToHeaders ValidatePipelineDefinition

instance ToJSON ValidatePipelineDefinition

-- | Contains the output from the ValidatePipelineDefinition action.
data ValidatePipelineDefinitionResponse = ValidatePipelineDefinitionResponse
    { _vpdrValidationErrors :: [ValidationError]
    , _vpdrValidationWarnings :: [ValidationWarning]
    , _vpdrErrored :: !Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ValidatePipelineDefinitionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ValidationErrors ::@ @[ValidationError]@
--
-- * @ValidationWarnings ::@ @[ValidationWarning]@
--
-- * @Errored ::@ @Bool@
--
validatePipelineDefinitionResponse :: Bool -- ^ 'vpdrErrored'
                                   -> ValidatePipelineDefinitionResponse
validatePipelineDefinitionResponse p3 = ValidatePipelineDefinitionResponse
    { _vpdrValidationErrors = mempty
    , _vpdrValidationWarnings = mempty
    , _vpdrErrored = p3
    }

-- | Lists the validation errors that were found by ValidatePipelineDefinition.
vpdrValidationErrors :: Lens' ValidatePipelineDefinitionResponse [ValidationError]
vpdrValidationErrors =
    lens _vpdrValidationErrors (\s a -> s { _vpdrValidationErrors = a })

-- | Lists the validation warnings that were found by
-- ValidatePipelineDefinition.
vpdrValidationWarnings :: Lens' ValidatePipelineDefinitionResponse [ValidationWarning]
vpdrValidationWarnings =
    lens _vpdrValidationWarnings (\s a -> s { _vpdrValidationWarnings = a })

-- | If True, there were validation errors.
vpdrErrored :: Lens' ValidatePipelineDefinitionResponse Bool
vpdrErrored = lens _vpdrErrored (\s a -> s { _vpdrErrored = a })

instance FromJSON ValidatePipelineDefinitionResponse

instance AWSRequest ValidatePipelineDefinition where
    type Sv ValidatePipelineDefinition = DataPipeline
    type Rs ValidatePipelineDefinition = ValidatePipelineDefinitionResponse

    request = get
    response _ = jsonResponse
