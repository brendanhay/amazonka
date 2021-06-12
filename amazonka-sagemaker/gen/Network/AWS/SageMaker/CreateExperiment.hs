{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateExperiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SageMaker /experiment/. An experiment is a collection of
-- /trials/ that are observed, compared and evaluated as a group. A trial
-- is a set of steps, called /trial components/, that produce a machine
-- learning model.
--
-- The goal of an experiment is to determine the components that produce
-- the best model. Multiple trials are performed, each one isolating and
-- measuring the impact of a change to one or more inputs, while keeping
-- the remaining inputs constant.
--
-- When you use Amazon SageMaker Studio or the Amazon SageMaker Python SDK,
-- all experiments, trials, and trial components are automatically tracked,
-- logged, and indexed. When you use the AWS SDK for Python (Boto), you
-- must use the logging APIs provided by the SDK.
--
-- You can add tags to experiments, trials, trial components and then use
-- the Search API to search for the tags.
--
-- To add a description to an experiment, specify the optional
-- @Description@ parameter. To add a description later, or to change the
-- description, call the UpdateExperiment API.
--
-- To get a list of all your experiments, call the ListExperiments API. To
-- view an experiment\'s properties, call the DescribeExperiment API. To
-- get a list of all the trials associated with an experiment, call the
-- ListTrials API. To create a trial call the CreateTrial API.
module Network.AWS.SageMaker.CreateExperiment
  ( -- * Creating a Request
    CreateExperiment (..),
    newCreateExperiment,

    -- * Request Lenses
    createExperiment_tags,
    createExperiment_description,
    createExperiment_displayName,
    createExperiment_experimentName,

    -- * Destructuring the Response
    CreateExperimentResponse (..),
    newCreateExperimentResponse,

    -- * Response Lenses
    createExperimentResponse_experimentArn,
    createExperimentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateExperiment' smart constructor.
data CreateExperiment = CreateExperiment'
  { -- | A list of tags to associate with the experiment. You can use Search API
    -- to search on the tags.
    tags :: Core.Maybe [Tag],
    -- | The description of the experiment.
    description :: Core.Maybe Core.Text,
    -- | The name of the experiment as displayed. The name doesn\'t need to be
    -- unique. If you don\'t specify @DisplayName@, the value in
    -- @ExperimentName@ is displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The name of the experiment. The name must be unique in your AWS account
    -- and is not case-sensitive.
    experimentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createExperiment_tags' - A list of tags to associate with the experiment. You can use Search API
-- to search on the tags.
--
-- 'description', 'createExperiment_description' - The description of the experiment.
--
-- 'displayName', 'createExperiment_displayName' - The name of the experiment as displayed. The name doesn\'t need to be
-- unique. If you don\'t specify @DisplayName@, the value in
-- @ExperimentName@ is displayed.
--
-- 'experimentName', 'createExperiment_experimentName' - The name of the experiment. The name must be unique in your AWS account
-- and is not case-sensitive.
newCreateExperiment ::
  -- | 'experimentName'
  Core.Text ->
  CreateExperiment
newCreateExperiment pExperimentName_ =
  CreateExperiment'
    { tags = Core.Nothing,
      description = Core.Nothing,
      displayName = Core.Nothing,
      experimentName = pExperimentName_
    }

-- | A list of tags to associate with the experiment. You can use Search API
-- to search on the tags.
createExperiment_tags :: Lens.Lens' CreateExperiment (Core.Maybe [Tag])
createExperiment_tags = Lens.lens (\CreateExperiment' {tags} -> tags) (\s@CreateExperiment' {} a -> s {tags = a} :: CreateExperiment) Core.. Lens.mapping Lens._Coerce

-- | The description of the experiment.
createExperiment_description :: Lens.Lens' CreateExperiment (Core.Maybe Core.Text)
createExperiment_description = Lens.lens (\CreateExperiment' {description} -> description) (\s@CreateExperiment' {} a -> s {description = a} :: CreateExperiment)

-- | The name of the experiment as displayed. The name doesn\'t need to be
-- unique. If you don\'t specify @DisplayName@, the value in
-- @ExperimentName@ is displayed.
createExperiment_displayName :: Lens.Lens' CreateExperiment (Core.Maybe Core.Text)
createExperiment_displayName = Lens.lens (\CreateExperiment' {displayName} -> displayName) (\s@CreateExperiment' {} a -> s {displayName = a} :: CreateExperiment)

-- | The name of the experiment. The name must be unique in your AWS account
-- and is not case-sensitive.
createExperiment_experimentName :: Lens.Lens' CreateExperiment Core.Text
createExperiment_experimentName = Lens.lens (\CreateExperiment' {experimentName} -> experimentName) (\s@CreateExperiment' {} a -> s {experimentName = a} :: CreateExperiment)

instance Core.AWSRequest CreateExperiment where
  type
    AWSResponse CreateExperiment =
      CreateExperimentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExperimentResponse'
            Core.<$> (x Core..?> "ExperimentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateExperiment

instance Core.NFData CreateExperiment

instance Core.ToHeaders CreateExperiment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.CreateExperiment" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateExperiment where
  toJSON CreateExperiment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("DisplayName" Core..=) Core.<$> displayName,
            Core.Just ("ExperimentName" Core..= experimentName)
          ]
      )

instance Core.ToPath CreateExperiment where
  toPath = Core.const "/"

instance Core.ToQuery CreateExperiment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateExperimentResponse' smart constructor.
data CreateExperimentResponse = CreateExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentArn', 'createExperimentResponse_experimentArn' - The Amazon Resource Name (ARN) of the experiment.
--
-- 'httpStatus', 'createExperimentResponse_httpStatus' - The response's http status code.
newCreateExperimentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateExperimentResponse
newCreateExperimentResponse pHttpStatus_ =
  CreateExperimentResponse'
    { experimentArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment.
createExperimentResponse_experimentArn :: Lens.Lens' CreateExperimentResponse (Core.Maybe Core.Text)
createExperimentResponse_experimentArn = Lens.lens (\CreateExperimentResponse' {experimentArn} -> experimentArn) (\s@CreateExperimentResponse' {} a -> s {experimentArn = a} :: CreateExperimentResponse)

-- | The response's http status code.
createExperimentResponse_httpStatus :: Lens.Lens' CreateExperimentResponse Core.Int
createExperimentResponse_httpStatus = Lens.lens (\CreateExperimentResponse' {httpStatus} -> httpStatus) (\s@CreateExperimentResponse' {} a -> s {httpStatus = a} :: CreateExperimentResponse)

instance Core.NFData CreateExperimentResponse
