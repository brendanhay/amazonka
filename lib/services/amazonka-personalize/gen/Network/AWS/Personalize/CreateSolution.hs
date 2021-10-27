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
-- Module      : Network.AWS.Personalize.CreateSolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the configuration for training a model. A trained model is known
-- as a solution. After the configuration is created, you train the model
-- (create a solution) by calling the CreateSolutionVersion operation.
-- Every time you call @CreateSolutionVersion@, a new version of the
-- solution is created.
--
-- After creating a solution version, you check its accuracy by calling
-- GetSolutionMetrics. When you are satisfied with the version, you deploy
-- it using CreateCampaign. The campaign provides recommendations to a
-- client through the
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
-- To get the status of the solution, call DescribeSolution. Wait until the
-- status shows as ACTIVE before calling @CreateSolutionVersion@.
--
-- __Related APIs__
--
-- -   ListSolutions
--
-- -   CreateSolutionVersion
--
-- -   DescribeSolution
--
-- -   DeleteSolution
--
-- -   ListSolutionVersions
--
-- -   DescribeSolutionVersion
module Network.AWS.Personalize.CreateSolution
  ( -- * Creating a Request
    CreateSolution (..),
    newCreateSolution,

    -- * Request Lenses
    createSolution_performAutoML,
    createSolution_recipeArn,
    createSolution_eventType,
    createSolution_solutionConfig,
    createSolution_performHPO,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Personalize.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSolution' smart constructor.
data CreateSolution = CreateSolution'
  { -- | Whether to perform automated machine learning (AutoML). The default is
    -- @false@. For this case, you must specify @recipeArn@.
    --
    -- When set to @true@, Amazon Personalize analyzes your training data and
    -- selects the optimal USER_PERSONALIZATION recipe and hyperparameters. In
    -- this case, you must omit @recipeArn@. Amazon Personalize determines the
    -- optimal recipe by running tests with different values for the
    -- hyperparameters. AutoML lengthens the training process as compared to
    -- selecting a specific recipe.
    performAutoML :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the recipe to use for model training. Only specified when
    -- @performAutoML@ is false.
    recipeArn :: Prelude.Maybe Prelude.Text,
    -- | When your have multiple event types (using an @EVENT_TYPE@ schema
    -- field), this parameter specifies which event type (for example,
    -- \'click\' or \'like\') is used for training the model.
    --
    -- If you do not provide an @eventType@, Amazon Personalize will use all
    -- interactions for training with equal weight regardless of type.
    eventType :: Prelude.Maybe Prelude.Text,
    -- | The configuration to use with the solution. When @performAutoML@ is set
    -- to true, Amazon Personalize only evaluates the @autoMLConfig@ section of
    -- the solution configuration.
    --
    -- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
    -- this time.
    solutionConfig :: Prelude.Maybe SolutionConfig,
    -- | Whether to perform hyperparameter optimization (HPO) on the specified or
    -- selected recipe. The default is @false@.
    --
    -- When performing AutoML, this parameter is always @true@ and you should
    -- not set it to @false@.
    performHPO :: Prelude.Maybe Prelude.Bool,
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
-- 'recipeArn', 'createSolution_recipeArn' - The ARN of the recipe to use for model training. Only specified when
-- @performAutoML@ is false.
--
-- 'eventType', 'createSolution_eventType' - When your have multiple event types (using an @EVENT_TYPE@ schema
-- field), this parameter specifies which event type (for example,
-- \'click\' or \'like\') is used for training the model.
--
-- If you do not provide an @eventType@, Amazon Personalize will use all
-- interactions for training with equal weight regardless of type.
--
-- 'solutionConfig', 'createSolution_solutionConfig' - The configuration to use with the solution. When @performAutoML@ is set
-- to true, Amazon Personalize only evaluates the @autoMLConfig@ section of
-- the solution configuration.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
--
-- 'performHPO', 'createSolution_performHPO' - Whether to perform hyperparameter optimization (HPO) on the specified or
-- selected recipe. The default is @false@.
--
-- When performing AutoML, this parameter is always @true@ and you should
-- not set it to @false@.
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
    { performAutoML = Prelude.Nothing,
      recipeArn = Prelude.Nothing,
      eventType = Prelude.Nothing,
      solutionConfig = Prelude.Nothing,
      performHPO = Prelude.Nothing,
      name = pName_,
      datasetGroupArn = pDatasetGroupArn_
    }

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

-- | The ARN of the recipe to use for model training. Only specified when
-- @performAutoML@ is false.
createSolution_recipeArn :: Lens.Lens' CreateSolution (Prelude.Maybe Prelude.Text)
createSolution_recipeArn = Lens.lens (\CreateSolution' {recipeArn} -> recipeArn) (\s@CreateSolution' {} a -> s {recipeArn = a} :: CreateSolution)

-- | When your have multiple event types (using an @EVENT_TYPE@ schema
-- field), this parameter specifies which event type (for example,
-- \'click\' or \'like\') is used for training the model.
--
-- If you do not provide an @eventType@, Amazon Personalize will use all
-- interactions for training with equal weight regardless of type.
createSolution_eventType :: Lens.Lens' CreateSolution (Prelude.Maybe Prelude.Text)
createSolution_eventType = Lens.lens (\CreateSolution' {eventType} -> eventType) (\s@CreateSolution' {} a -> s {eventType = a} :: CreateSolution)

-- | The configuration to use with the solution. When @performAutoML@ is set
-- to true, Amazon Personalize only evaluates the @autoMLConfig@ section of
-- the solution configuration.
--
-- Amazon Personalize doesn\'t support configuring the @hpoObjective@ at
-- this time.
createSolution_solutionConfig :: Lens.Lens' CreateSolution (Prelude.Maybe SolutionConfig)
createSolution_solutionConfig = Lens.lens (\CreateSolution' {solutionConfig} -> solutionConfig) (\s@CreateSolution' {} a -> s {solutionConfig = a} :: CreateSolution)

-- | Whether to perform hyperparameter optimization (HPO) on the specified or
-- selected recipe. The default is @false@.
--
-- When performing AutoML, this parameter is always @true@ and you should
-- not set it to @false@.
createSolution_performHPO :: Lens.Lens' CreateSolution (Prelude.Maybe Prelude.Bool)
createSolution_performHPO = Lens.lens (\CreateSolution' {performHPO} -> performHPO) (\s@CreateSolution' {} a -> s {performHPO = a} :: CreateSolution)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSolutionResponse'
            Prelude.<$> (x Core..?> "solutionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSolution

instance Prelude.NFData CreateSolution

instance Core.ToHeaders CreateSolution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.CreateSolution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSolution where
  toJSON CreateSolution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("performAutoML" Core..=) Prelude.<$> performAutoML,
            ("recipeArn" Core..=) Prelude.<$> recipeArn,
            ("eventType" Core..=) Prelude.<$> eventType,
            ("solutionConfig" Core..=)
              Prelude.<$> solutionConfig,
            ("performHPO" Core..=) Prelude.<$> performHPO,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("datasetGroupArn" Core..= datasetGroupArn)
          ]
      )

instance Core.ToPath CreateSolution where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSolution where
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

instance Prelude.NFData CreateSolutionResponse
