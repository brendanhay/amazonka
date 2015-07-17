{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | AWS CodePipeline __Overview__
--
-- This is the AWS CodePipeline API Reference. This guide provides
-- descriptions of the actions and data types for AWS CodePipeline. Some
-- functionality for your pipeline is only configurable through the API.
-- For additional information, see the
-- <http://docs.aws.amazon.com/pipelines/latest/userguide/welcome.html AWS CodePipeline User Guide>.
--
-- You can use the AWS CodePipeline API to work with pipelines, stages,
-- actions, gates, and transitions, as described below.
--
-- /Pipelines/ are models of automated release processes. Each pipeline is
-- uniquely named, and consists of actions, gates, and stages.
--
-- You can work with pipelines by calling:
--
-- -   CreatePipeline, which creates a uniquely-named pipeline.
-- -   DeletePipeline, which deletes the specified pipeline.
-- -   GetPipeline, which returns information about a pipeline structure.
-- -   GetPipelineState, which returns information about the current state
--     of the stages and actions of a pipeline.
-- -   ListPipelines, which gets a summary of all of the pipelines
--     associated with your account.
-- -   StartPipelineExecution, which runs the the most recent revision of
--     an artifact through the pipeline.
-- -   UpdatePipeline, which updates a pipeline with edits or changes to
--     the structure of the pipeline.
--
-- Pipelines include /stages/, which are which are logical groupings of
-- gates and actions. Each stage contains one or more actions that must
-- complete before the next stage begins. A stage will result in success or
-- failure. If a stage fails, then the pipeline stops at that stage and
-- will remain stopped until either a new version of an artifact appears in
-- the source location, or a user takes action to re-run the most recent
-- artifact through the pipeline. You can call GetPipelineState, which
-- displays the status of a pipeline, including the status of stages in the
-- pipeline, or GetPipeline, which returns the entire structure of the
-- pipeline, including the stages of that pipeline.
--
-- Pipeline stages include /actions/, which are categorized into categories
-- such as source or build actions performed within a stage of a pipeline.
-- For example, you can use a source action to import artifacts into a
-- pipeline from a source such as Amazon S3. Like stages, you do not work
-- with actions directly in most cases, but you do define and interact with
-- actions when working with pipeline operations such as CreatePipeline and
-- GetPipelineState.
--
-- Pipelines also include /transitions/, which allow the transition of
-- artifacts from one stage to the next in a pipeline after the actions in
-- one stage complete.
--
-- You can work with transitions by calling:
--
-- -   DisableStageTransition, which prevents artifacts from transitioning
--     to the next stage in a pipeline.
-- -   EnableStageTransition, which enables transition of artifacts between
--     stages in a pipeline.
--
-- __Using the API to integrate with AWS CodePipeline__
--
-- For third-party integrators or developers who want to create their own
-- integrations with AWS CodePipeline, the expected sequence varies from
-- the standard API user. In order to integrate with AWS CodePipeline,
-- developers will need to work with the following items:
--
-- -   Jobs, which are instances of an action. For example, a job for a
--     source action might import a revision of an artifact from a source.
--
--     You can work with jobs by calling:
--
--     -   AcknowledgeJob, which confirms whether a job worker has received
--         the specified job,
--     -   PollForJobs, which determines whether there are any jobs to act
--         upon,
--     -   PutJobFailureResult, which provides details of a job failure,
--         and
--     -   PutJobSuccessResult, which provides details of a job success.
-- -   Third party jobs, which are instances of an action created by a
--     partner action and integrated into AWS CodePipeline. Partner actions
--     are created by members of the AWS Partner Network.
--
--     You can work with third party jobs by calling:
--
--     -   AcknowledgeThirdPartyJob, which confirms whether a job worker
--         has received the specified job,
--     -   PollForThirdPartyJobs, which determines whether there are any
--         jobs to act upon,
--     -   PutThirdPartyJobFailureResult, which provides details of a job
--         failure, and
--     -   PutThirdPartyJobSuccessResult, which provides details of a job
--         success.
module Network.AWS.CodePipeline
    ( module Export
    ) where

import           Network.AWS.CodePipeline.AcknowledgeJob                as Export
import           Network.AWS.CodePipeline.AcknowledgeThirdPartyJob      as Export
import           Network.AWS.CodePipeline.CreateCustomActionType        as Export
import           Network.AWS.CodePipeline.CreatePipeline                as Export
import           Network.AWS.CodePipeline.DeleteCustomActionType        as Export
import           Network.AWS.CodePipeline.DeletePipeline                as Export
import           Network.AWS.CodePipeline.DisableStageTransition        as Export
import           Network.AWS.CodePipeline.EnableStageTransition         as Export
import           Network.AWS.CodePipeline.GetJobDetails                 as Export
import           Network.AWS.CodePipeline.GetPipeline                   as Export
import           Network.AWS.CodePipeline.GetPipelineState              as Export
import           Network.AWS.CodePipeline.GetThirdPartyJobDetails       as Export
import           Network.AWS.CodePipeline.ListActionTypes               as Export
import           Network.AWS.CodePipeline.ListPipelines                 as Export
import           Network.AWS.CodePipeline.PollForJobs                   as Export
import           Network.AWS.CodePipeline.PollForThirdPartyJobs         as Export
import           Network.AWS.CodePipeline.PutActionRevision             as Export
import           Network.AWS.CodePipeline.PutJobFailureResult           as Export
import           Network.AWS.CodePipeline.PutJobSuccessResult           as Export
import           Network.AWS.CodePipeline.PutThirdPartyJobFailureResult as Export
import           Network.AWS.CodePipeline.PutThirdPartyJobSuccessResult as Export
import           Network.AWS.CodePipeline.StartPipelineExecution        as Export
import           Network.AWS.CodePipeline.Types                         as Export
import           Network.AWS.CodePipeline.Types.Product                 as Export
import           Network.AWS.CodePipeline.Types.Sum                     as Export
import           Network.AWS.CodePipeline.UpdatePipeline                as Export
import           Network.AWS.CodePipeline.Waiters                       as Export
