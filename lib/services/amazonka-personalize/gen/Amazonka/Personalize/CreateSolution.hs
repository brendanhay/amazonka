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
-- Module      : Amazonka.Personalize.CreateSolution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the configuration for training a model. A trained model is known
-- as a solution. After the configuration is created, you train the model
-- (create a solution) by calling the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolutionVersion.html CreateSolutionVersion>
-- operation. Every time you call @CreateSolutionVersion@, a new version of
-- the solution is created.
--
-- After creating a solution version, you check its accuracy by calling
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_GetSolutionMetrics.html GetSolutionMetrics>.
-- When you are satisfied with the version, you deploy it using
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateCampaign.html CreateCampaign>.
-- The campaign provides recommendations to a client through the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations>
-- API.
--
-- To train a model, Amazon Personalize requires training data and a
-- recipe. The training data comes from the dataset group that you provide
-- in the request. A recipe specifies the training algorithm and a feature
-- transformation. You can specify one of the predefined recipes provided
-- by Amazon Personalize. Alternatively, you can specify @performAutoML@
-- and Amazon Personalize will analyze your data and select the optimum
-- USER_PERSONALIZATION recipe for you.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ for
-- solution hyperparameter optimization at this time.
--
-- __Status__
--
-- A solution can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- To get the status of the solution, call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeSolution.html DescribeSolution>.
-- Wait until the status shows as ACTIVE before calling
-- @CreateSolutionVersion@.
--
-- __Related APIs__
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_ListSolutions.html ListSolutions>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateSolutionVersion.html CreateSolutionVersion>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeSolution.html DescribeSolution>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DeleteSolution.html DeleteSolution>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_ListSolutionVersions.html ListSolutionVersions>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeSolutionVersion.html DescribeSolutionVersion>
module Amazonka.Personalize.CreateSolution
  ( -- * Creating a Request
    CreateSolution (..),
    newCreateSolution,

    -- * Request Lenses
    createSolution_tags,
    createSolution_eventType,
    createSolution_performAutoML,
    createSolution_performHPO,
    createSolution_solutionConfig,
    createSolution_recipeArn,
    createSolution_name,
    createSolution_datasetGroupArn,

    -- * Destructuring the Response
    CreateSolutionResponse (..),
    newCreateSolutionResponse,

    -- * Response Lenses
    createSolutionResponse_solutionArn,
    createSolutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSolution' smart constructor.
data CreateSolution = CreateSolution'
  { -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
    -- to apply to the solution.
    tags :: Prelude.Maybe [Tag],
    -- | When your have multiple event types (using an @EVENT_TYPE@ schema
    -- field), this parameter specifies which event type (for example,
    -- \'click\' or \'like\') is used for training the model.
    --
    -- If you do not provide an @eventType@, Amazon Personalize will use all
    -- interactions for training with equal weight regardless of type.
    eventType :: Prelude.Maybe Prelude.Text,
    -- | Whether to perform automated machine learning (AutoML). The default is
    -- @false@. For this case, you must specify @recipeArn@.
    --
    -- When set to @true@, Amazon Personalize analyzes your training data and
    -- selects the optimal USER_PERSONALIZATION recipe and hyperparameters. In
    -- this case, you must omit @recipeArn@. Amazon Personalize determines the
    -- optimal recipe by running tests with different values for the
    -- hyperparameters. AutoML lengthens the training process as compared to
    -- selecting a specific recipe.
    performAutoML :: Prelude.Maybe Prelude.Bool,
    -- | Whether to perform hyperparameter optimization (HPO) on the specified or
    -- selected recipe. The default is @false@.
    --
    -- When performing AutoML, this parameter is always @true@ and you should
    -- not set it to @false@.
    performHPO :: Prelude.Maybe Prelude.Bool,
    -- | The configuration to use with the solution. When @performAutoML@ is set
    -- to true, Amazon Personalize only evaluates the @autoMLConfig@ section of
    -- the solution configuration.
    --
    -- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
    -- this time.
    solutionConfig :: Prelude.Maybe SolutionConfig,
    -- | The ARN of the recipe to use for model training. Only specified when
    -- @performAutoML@ is false.
    recipeArn :: Prelude.Maybe Prelude.Text,
    -- | The name for the solution.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset group that provides the
    -- training data.
    datasetGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSolution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSolution_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the solution.
--
-- 'eventType', 'createSolution_eventType' - When your have multiple event types (using an @EVENT_TYPE@ schema
-- field), this parameter specifies which event type (for example,
-- \'click\' or \'like\') is used for training the model.
--
-- If you do not provide an @eventType@, Amazon Personalize will use all
-- interactions for training with equal weight regardless of type.
--
-- 'performAutoML', 'createSolution_performAutoML' - Whether to perform automated machine learning (AutoML). The default is
-- @false@. For this case, you must specify @recipeArn@.
--
-- When set to @true@, Amazon Personalize analyzes your training data and
-- selects the optimal USER_PERSONALIZATION recipe and hyperparameters. In
-- this case, you must omit @recipeArn@. Amazon Personalize determines the
-- optimal recipe by running tests with different values for the
-- hyperparameters. AutoML lengthens the training process as compared to
-- selecting a specific recipe.
--
-- 'performHPO', 'createSolution_performHPO' - Whether to perform hyperparameter optimization (HPO) on the specified or
-- selected recipe. The default is @false@.
--
-- When performing AutoML, this parameter is always @true@ and you should
-- not set it to @false@.
--
-- 'solutionConfig', 'createSolution_solutionConfig' - The configuration to use with the solution. When @performAutoML@ is set
-- to true, Amazon Personalize only evaluates the @autoMLConfig@ section of
-- the solution configuration.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
--
-- 'recipeArn', 'createSolution_recipeArn' - The ARN of the recipe to use for model training. Only specified when
-- @performAutoML@ is false.
--
-- 'name', 'createSolution_name' - The name for the solution.
--
-- 'datasetGroupArn', 'createSolution_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group that provides the
-- training data.
newCreateSolution ::
  -- | 'name'
  Prelude.Text ->
  -- | 'datasetGroupArn'
  Prelude.Text ->
  CreateSolution
newCreateSolution pName_ pDatasetGroupArn_ =
  CreateSolution'
    { tags = Prelude.Nothing,
      eventType = Prelude.Nothing,
      performAutoML = Prelude.Nothing,
      performHPO = Prelude.Nothing,
      solutionConfig = Prelude.Nothing,
      recipeArn = Prelude.Nothing,
      name = pName_,
      datasetGroupArn = pDatasetGroupArn_
    }

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the solution.
createSolution_tags :: Lens.Lens' CreateSolution (Prelude.Maybe [Tag])
createSolution_tags = Lens.lens (\CreateSolution' {tags} -> tags) (\s@CreateSolution' {} a -> s {tags = a} :: CreateSolution) Prelude.. Lens.mapping Lens.coerced

-- | When your have multiple event types (using an @EVENT_TYPE@ schema
-- field), this parameter specifies which event type (for example,
-- \'click\' or \'like\') is used for training the model.
--
-- If you do not provide an @eventType@, Amazon Personalize will use all
-- interactions for training with equal weight regardless of type.
createSolution_eventType :: Lens.Lens' CreateSolution (Prelude.Maybe Prelude.Text)
createSolution_eventType = Lens.lens (\CreateSolution' {eventType} -> eventType) (\s@CreateSolution' {} a -> s {eventType = a} :: CreateSolution)

-- | Whether to perform automated machine learning (AutoML). The default is
-- @false@. For this case, you must specify @recipeArn@.
--
-- When set to @true@, Amazon Personalize analyzes your training data and
-- selects the optimal USER_PERSONALIZATION recipe and hyperparameters. In
-- this case, you must omit @recipeArn@. Amazon Personalize determines the
-- optimal recipe by running tests with different values for the
-- hyperparameters. AutoML lengthens the training process as compared to
-- selecting a specific recipe.
createSolution_performAutoML :: Lens.Lens' CreateSolution (Prelude.Maybe Prelude.Bool)
createSolution_performAutoML = Lens.lens (\CreateSolution' {performAutoML} -> performAutoML) (\s@CreateSolution' {} a -> s {performAutoML = a} :: CreateSolution)

-- | Whether to perform hyperparameter optimization (HPO) on the specified or
-- selected recipe. The default is @false@.
--
-- When performing AutoML, this parameter is always @true@ and you should
-- not set it to @false@.
createSolution_performHPO :: Lens.Lens' CreateSolution (Prelude.Maybe Prelude.Bool)
createSolution_performHPO = Lens.lens (\CreateSolution' {performHPO} -> performHPO) (\s@CreateSolution' {} a -> s {performHPO = a} :: CreateSolution)

-- | The configuration to use with the solution. When @performAutoML@ is set
-- to true, Amazon Personalize only evaluates the @autoMLConfig@ section of
-- the solution configuration.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
createSolution_solutionConfig :: Lens.Lens' CreateSolution (Prelude.Maybe SolutionConfig)
createSolution_solutionConfig = Lens.lens (\CreateSolution' {solutionConfig} -> solutionConfig) (\s@CreateSolution' {} a -> s {solutionConfig = a} :: CreateSolution)

-- | The ARN of the recipe to use for model training. Only specified when
-- @performAutoML@ is false.
createSolution_recipeArn :: Lens.Lens' CreateSolution (Prelude.Maybe Prelude.Text)
createSolution_recipeArn = Lens.lens (\CreateSolution' {recipeArn} -> recipeArn) (\s@CreateSolution' {} a -> s {recipeArn = a} :: CreateSolution)

-- | The name for the solution.
createSolution_name :: Lens.Lens' CreateSolution Prelude.Text
createSolution_name = Lens.lens (\CreateSolution' {name} -> name) (\s@CreateSolution' {} a -> s {name = a} :: CreateSolution)

-- | The Amazon Resource Name (ARN) of the dataset group that provides the
-- training data.
createSolution_datasetGroupArn :: Lens.Lens' CreateSolution Prelude.Text
createSolution_datasetGroupArn = Lens.lens (\CreateSolution' {datasetGroupArn} -> datasetGroupArn) (\s@CreateSolution' {} a -> s {datasetGroupArn = a} :: CreateSolution)

instance Core.AWSRequest CreateSolution where
  type
    AWSResponse CreateSolution =
      CreateSolutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSolutionResponse'
            Prelude.<$> (x Data..?> "solutionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSolution where
  hashWithSalt _salt CreateSolution' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` performAutoML
      `Prelude.hashWithSalt` performHPO
      `Prelude.hashWithSalt` solutionConfig
      `Prelude.hashWithSalt` recipeArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` datasetGroupArn

instance Prelude.NFData CreateSolution where
  rnf CreateSolution' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf performAutoML
      `Prelude.seq` Prelude.rnf performHPO
      `Prelude.seq` Prelude.rnf solutionConfig
      `Prelude.seq` Prelude.rnf recipeArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf datasetGroupArn

instance Data.ToHeaders CreateSolution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateSolution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSolution where
  toJSON CreateSolution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("eventType" Data..=) Prelude.<$> eventType,
            ("performAutoML" Data..=) Prelude.<$> performAutoML,
            ("performHPO" Data..=) Prelude.<$> performHPO,
            ("solutionConfig" Data..=)
              Prelude.<$> solutionConfig,
            ("recipeArn" Data..=) Prelude.<$> recipeArn,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("datasetGroupArn" Data..= datasetGroupArn)
          ]
      )

instance Data.ToPath CreateSolution where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSolution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSolutionResponse' smart constructor.
data CreateSolutionResponse = CreateSolutionResponse'
  { -- | The ARN of the solution.
    solutionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSolutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionArn', 'createSolutionResponse_solutionArn' - The ARN of the solution.
--
-- 'httpStatus', 'createSolutionResponse_httpStatus' - The response's http status code.
newCreateSolutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSolutionResponse
newCreateSolutionResponse pHttpStatus_ =
  CreateSolutionResponse'
    { solutionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the solution.
createSolutionResponse_solutionArn :: Lens.Lens' CreateSolutionResponse (Prelude.Maybe Prelude.Text)
createSolutionResponse_solutionArn = Lens.lens (\CreateSolutionResponse' {solutionArn} -> solutionArn) (\s@CreateSolutionResponse' {} a -> s {solutionArn = a} :: CreateSolutionResponse)

-- | The response's http status code.
createSolutionResponse_httpStatus :: Lens.Lens' CreateSolutionResponse Prelude.Int
createSolutionResponse_httpStatus = Lens.lens (\CreateSolutionResponse' {httpStatus} -> httpStatus) (\s@CreateSolutionResponse' {} a -> s {httpStatus = a} :: CreateSolutionResponse)

instance Prelude.NFData CreateSolutionResponse where
  rnf CreateSolutionResponse' {..} =
    Prelude.rnf solutionArn
      `Prelude.seq` Prelude.rnf httpStatus
