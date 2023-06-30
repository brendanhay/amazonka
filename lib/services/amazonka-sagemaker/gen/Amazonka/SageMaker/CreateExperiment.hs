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
-- Module      : Amazonka.SageMaker.CreateExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a SageMaker /experiment/. An experiment is a collection of
-- /trials/ that are observed, compared and evaluated as a group. A trial
-- is a set of steps, called /trial components/, that produce a machine
-- learning model.
--
-- In the Studio UI, trials are referred to as /run groups/ and trial
-- components are referred to as /runs/.
--
-- The goal of an experiment is to determine the components that produce
-- the best model. Multiple trials are performed, each one isolating and
-- measuring the impact of a change to one or more inputs, while keeping
-- the remaining inputs constant.
--
-- When you use SageMaker Studio or the SageMaker Python SDK, all
-- experiments, trials, and trial components are automatically tracked,
-- logged, and indexed. When you use the Amazon Web Services SDK for Python
-- (Boto), you must use the logging APIs provided by the SDK.
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
module Amazonka.SageMaker.CreateExperiment
  ( -- * Creating a Request
    CreateExperiment (..),
    newCreateExperiment,

    -- * Request Lenses
    createExperiment_description,
    createExperiment_displayName,
    createExperiment_tags,
    createExperiment_experimentName,

    -- * Destructuring the Response
    CreateExperimentResponse (..),
    newCreateExperimentResponse,

    -- * Response Lenses
    createExperimentResponse_experimentArn,
    createExperimentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateExperiment' smart constructor.
data CreateExperiment = CreateExperiment'
  { -- | The description of the experiment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the experiment as displayed. The name doesn\'t need to be
    -- unique. If you don\'t specify @DisplayName@, the value in
    -- @ExperimentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to associate with the experiment. You can use Search API
    -- to search on the tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the experiment. The name must be unique in your Amazon Web
    -- Services account and is not case-sensitive.
    experimentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createExperiment_description' - The description of the experiment.
--
-- 'displayName', 'createExperiment_displayName' - The name of the experiment as displayed. The name doesn\'t need to be
-- unique. If you don\'t specify @DisplayName@, the value in
-- @ExperimentName@ is displayed.
--
-- 'tags', 'createExperiment_tags' - A list of tags to associate with the experiment. You can use Search API
-- to search on the tags.
--
-- 'experimentName', 'createExperiment_experimentName' - The name of the experiment. The name must be unique in your Amazon Web
-- Services account and is not case-sensitive.
newCreateExperiment ::
  -- | 'experimentName'
  Prelude.Text ->
  CreateExperiment
newCreateExperiment pExperimentName_ =
  CreateExperiment'
    { description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      tags = Prelude.Nothing,
      experimentName = pExperimentName_
    }

-- | The description of the experiment.
createExperiment_description :: Lens.Lens' CreateExperiment (Prelude.Maybe Prelude.Text)
createExperiment_description = Lens.lens (\CreateExperiment' {description} -> description) (\s@CreateExperiment' {} a -> s {description = a} :: CreateExperiment)

-- | The name of the experiment as displayed. The name doesn\'t need to be
-- unique. If you don\'t specify @DisplayName@, the value in
-- @ExperimentName@ is displayed.
createExperiment_displayName :: Lens.Lens' CreateExperiment (Prelude.Maybe Prelude.Text)
createExperiment_displayName = Lens.lens (\CreateExperiment' {displayName} -> displayName) (\s@CreateExperiment' {} a -> s {displayName = a} :: CreateExperiment)

-- | A list of tags to associate with the experiment. You can use Search API
-- to search on the tags.
createExperiment_tags :: Lens.Lens' CreateExperiment (Prelude.Maybe [Tag])
createExperiment_tags = Lens.lens (\CreateExperiment' {tags} -> tags) (\s@CreateExperiment' {} a -> s {tags = a} :: CreateExperiment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the experiment. The name must be unique in your Amazon Web
-- Services account and is not case-sensitive.
createExperiment_experimentName :: Lens.Lens' CreateExperiment Prelude.Text
createExperiment_experimentName = Lens.lens (\CreateExperiment' {experimentName} -> experimentName) (\s@CreateExperiment' {} a -> s {experimentName = a} :: CreateExperiment)

instance Core.AWSRequest CreateExperiment where
  type
    AWSResponse CreateExperiment =
      CreateExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExperimentResponse'
            Prelude.<$> (x Data..?> "ExperimentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateExperiment where
  hashWithSalt _salt CreateExperiment' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` experimentName

instance Prelude.NFData CreateExperiment where
  rnf CreateExperiment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf experimentName

instance Data.ToHeaders CreateExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateExperiment" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateExperiment where
  toJSON CreateExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ExperimentName" Data..= experimentName)
          ]
      )

instance Data.ToPath CreateExperiment where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateExperimentResponse' smart constructor.
data CreateExperimentResponse = CreateExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment.
    experimentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateExperimentResponse
newCreateExperimentResponse pHttpStatus_ =
  CreateExperimentResponse'
    { experimentArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment.
createExperimentResponse_experimentArn :: Lens.Lens' CreateExperimentResponse (Prelude.Maybe Prelude.Text)
createExperimentResponse_experimentArn = Lens.lens (\CreateExperimentResponse' {experimentArn} -> experimentArn) (\s@CreateExperimentResponse' {} a -> s {experimentArn = a} :: CreateExperimentResponse)

-- | The response's http status code.
createExperimentResponse_httpStatus :: Lens.Lens' CreateExperimentResponse Prelude.Int
createExperimentResponse_httpStatus = Lens.lens (\CreateExperimentResponse' {httpStatus} -> httpStatus) (\s@CreateExperimentResponse' {} a -> s {httpStatus = a} :: CreateExperimentResponse)

instance Prelude.NFData CreateExperimentResponse where
  rnf CreateExperimentResponse' {..} =
    Prelude.rnf experimentArn
      `Prelude.seq` Prelude.rnf httpStatus
