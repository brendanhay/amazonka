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
-- Module      : Amazonka.Personalize.CreateRecommender
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a recommender with the recipe (a Domain dataset group use case)
-- you specify. You create recommenders for a Domain dataset group and
-- specify the recommender\'s Amazon Resource Name (ARN) when you make a
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_RS_GetRecommendations.html GetRecommendations>
-- request.
--
-- __Minimum recommendation requests per second__
--
-- When you create a recommender, you can configure the recommender\'s
-- minimum recommendation requests per second. The minimum recommendation
-- requests per second (@minRecommendationRequestsPerSecond@) specifies the
-- baseline recommendation request throughput provisioned by Amazon
-- Personalize. The default minRecommendationRequestsPerSecond is @1@. A
-- recommendation request is a single @GetRecommendations@ operation.
-- Request throughput is measured in requests per second and Amazon
-- Personalize uses your requests per second to derive your requests per
-- hour and the price of your recommender usage.
--
-- If your requests per second increases beyond
-- @minRecommendationRequestsPerSecond@, Amazon Personalize auto-scales the
-- provisioned capacity up and down, but never below
-- @minRecommendationRequestsPerSecond@. There\'s a short time delay while
-- the capacity is increased that might cause loss of requests.
--
-- Your bill is the greater of either the minimum requests per hour (based
-- on minRecommendationRequestsPerSecond) or the actual number of requests.
-- The actual request throughput used is calculated as the average
-- requests\/second within a one-hour window. We recommend starting with
-- the default @minRecommendationRequestsPerSecond@, track your usage using
-- Amazon CloudWatch metrics, and then increase the
-- @minRecommendationRequestsPerSecond@ as necessary.
--
-- __Status__
--
-- A recommender can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   STOP PENDING > STOP IN_PROGRESS > INACTIVE > START PENDING > START
--     IN_PROGRESS > ACTIVE
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- To get the recommender status, call
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeRecommender.html DescribeRecommender>.
--
-- Wait until the @status@ of the recommender is @ACTIVE@ before asking the
-- recommender for recommendations.
--
-- __Related APIs__
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_ListRecommenders.html ListRecommenders>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeRecommender.html DescribeRecommender>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_UpdateRecommender.html UpdateRecommender>
--
-- -   <https://docs.aws.amazon.com/personalize/latest/dg/API_DeleteRecommender.html DeleteRecommender>
module Amazonka.Personalize.CreateRecommender
  ( -- * Creating a Request
    CreateRecommender (..),
    newCreateRecommender,

    -- * Request Lenses
    createRecommender_recommenderConfig,
    createRecommender_tags,
    createRecommender_name,
    createRecommender_datasetGroupArn,
    createRecommender_recipeArn,

    -- * Destructuring the Response
    CreateRecommenderResponse (..),
    newCreateRecommenderResponse,

    -- * Response Lenses
    createRecommenderResponse_recommenderArn,
    createRecommenderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRecommender' smart constructor.
data CreateRecommender = CreateRecommender'
  { -- | The configuration details of the recommender.
    recommenderConfig :: Prelude.Maybe RecommenderConfig,
    -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
    -- to apply to the recommender.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the recommender.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the destination domain dataset group
    -- for the recommender.
    datasetGroupArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the recipe that the recommender will
    -- use. For a recommender, a recipe is a Domain dataset group use case.
    -- Only Domain dataset group use cases can be used to create a recommender.
    -- For information about use cases see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/domain-use-cases.html Choosing recommender use cases>.
    recipeArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRecommender' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderConfig', 'createRecommender_recommenderConfig' - The configuration details of the recommender.
--
-- 'tags', 'createRecommender_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the recommender.
--
-- 'name', 'createRecommender_name' - The name of the recommender.
--
-- 'datasetGroupArn', 'createRecommender_datasetGroupArn' - The Amazon Resource Name (ARN) of the destination domain dataset group
-- for the recommender.
--
-- 'recipeArn', 'createRecommender_recipeArn' - The Amazon Resource Name (ARN) of the recipe that the recommender will
-- use. For a recommender, a recipe is a Domain dataset group use case.
-- Only Domain dataset group use cases can be used to create a recommender.
-- For information about use cases see
-- <https://docs.aws.amazon.com/personalize/latest/dg/domain-use-cases.html Choosing recommender use cases>.
newCreateRecommender ::
  -- | 'name'
  Prelude.Text ->
  -- | 'datasetGroupArn'
  Prelude.Text ->
  -- | 'recipeArn'
  Prelude.Text ->
  CreateRecommender
newCreateRecommender
  pName_
  pDatasetGroupArn_
  pRecipeArn_ =
    CreateRecommender'
      { recommenderConfig =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        datasetGroupArn = pDatasetGroupArn_,
        recipeArn = pRecipeArn_
      }

-- | The configuration details of the recommender.
createRecommender_recommenderConfig :: Lens.Lens' CreateRecommender (Prelude.Maybe RecommenderConfig)
createRecommender_recommenderConfig = Lens.lens (\CreateRecommender' {recommenderConfig} -> recommenderConfig) (\s@CreateRecommender' {} a -> s {recommenderConfig = a} :: CreateRecommender)

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the recommender.
createRecommender_tags :: Lens.Lens' CreateRecommender (Prelude.Maybe [Tag])
createRecommender_tags = Lens.lens (\CreateRecommender' {tags} -> tags) (\s@CreateRecommender' {} a -> s {tags = a} :: CreateRecommender) Prelude.. Lens.mapping Lens.coerced

-- | The name of the recommender.
createRecommender_name :: Lens.Lens' CreateRecommender Prelude.Text
createRecommender_name = Lens.lens (\CreateRecommender' {name} -> name) (\s@CreateRecommender' {} a -> s {name = a} :: CreateRecommender)

-- | The Amazon Resource Name (ARN) of the destination domain dataset group
-- for the recommender.
createRecommender_datasetGroupArn :: Lens.Lens' CreateRecommender Prelude.Text
createRecommender_datasetGroupArn = Lens.lens (\CreateRecommender' {datasetGroupArn} -> datasetGroupArn) (\s@CreateRecommender' {} a -> s {datasetGroupArn = a} :: CreateRecommender)

-- | The Amazon Resource Name (ARN) of the recipe that the recommender will
-- use. For a recommender, a recipe is a Domain dataset group use case.
-- Only Domain dataset group use cases can be used to create a recommender.
-- For information about use cases see
-- <https://docs.aws.amazon.com/personalize/latest/dg/domain-use-cases.html Choosing recommender use cases>.
createRecommender_recipeArn :: Lens.Lens' CreateRecommender Prelude.Text
createRecommender_recipeArn = Lens.lens (\CreateRecommender' {recipeArn} -> recipeArn) (\s@CreateRecommender' {} a -> s {recipeArn = a} :: CreateRecommender)

instance Core.AWSRequest CreateRecommender where
  type
    AWSResponse CreateRecommender =
      CreateRecommenderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRecommenderResponse'
            Prelude.<$> (x Data..?> "recommenderArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRecommender where
  hashWithSalt _salt CreateRecommender' {..} =
    _salt `Prelude.hashWithSalt` recommenderConfig
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` recipeArn

instance Prelude.NFData CreateRecommender where
  rnf CreateRecommender' {..} =
    Prelude.rnf recommenderConfig
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf recipeArn

instance Data.ToHeaders CreateRecommender where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateRecommender" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRecommender where
  toJSON CreateRecommender' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("recommenderConfig" Data..=)
              Prelude.<$> recommenderConfig,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("datasetGroupArn" Data..= datasetGroupArn),
            Prelude.Just ("recipeArn" Data..= recipeArn)
          ]
      )

instance Data.ToPath CreateRecommender where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRecommender where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRecommenderResponse' smart constructor.
data CreateRecommenderResponse = CreateRecommenderResponse'
  { -- | The Amazon Resource Name (ARN) of the recommender.
    recommenderArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRecommenderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommenderArn', 'createRecommenderResponse_recommenderArn' - The Amazon Resource Name (ARN) of the recommender.
--
-- 'httpStatus', 'createRecommenderResponse_httpStatus' - The response's http status code.
newCreateRecommenderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRecommenderResponse
newCreateRecommenderResponse pHttpStatus_ =
  CreateRecommenderResponse'
    { recommenderArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the recommender.
createRecommenderResponse_recommenderArn :: Lens.Lens' CreateRecommenderResponse (Prelude.Maybe Prelude.Text)
createRecommenderResponse_recommenderArn = Lens.lens (\CreateRecommenderResponse' {recommenderArn} -> recommenderArn) (\s@CreateRecommenderResponse' {} a -> s {recommenderArn = a} :: CreateRecommenderResponse)

-- | The response's http status code.
createRecommenderResponse_httpStatus :: Lens.Lens' CreateRecommenderResponse Prelude.Int
createRecommenderResponse_httpStatus = Lens.lens (\CreateRecommenderResponse' {httpStatus} -> httpStatus) (\s@CreateRecommenderResponse' {} a -> s {httpStatus = a} :: CreateRecommenderResponse)

instance Prelude.NFData CreateRecommenderResponse where
  rnf CreateRecommenderResponse' {..} =
    Prelude.rnf recommenderArn
      `Prelude.seq` Prelude.rnf httpStatus
