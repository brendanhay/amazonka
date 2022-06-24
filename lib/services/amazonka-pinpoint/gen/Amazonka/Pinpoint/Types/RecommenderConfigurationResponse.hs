{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types.RecommenderConfigurationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.RecommenderConfigurationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about Amazon Pinpoint configuration settings for
-- retrieving and processing data from a recommender model.
--
-- /See:/ 'newRecommenderConfigurationResponse' smart constructor.
data RecommenderConfigurationResponse = RecommenderConfigurationResponse'
  { -- | The custom name of the configuration for the recommender model.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
    -- Amazon Pinpoint invokes to perform additional processing of
    -- recommendation data that it retrieves from the recommender model.
    recommendationTransformerUri :: Prelude.Maybe Prelude.Text,
    -- | The custom description of the configuration for the recommender model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The custom display name for the standard endpoint or user attribute
    -- (RecommendationItems) that temporarily stores recommended items for each
    -- endpoint or user, depending on the value for the
    -- RecommendationProviderIdType property. This name appears in the
    -- __Attribute finder__ of the template editor on the Amazon Pinpoint
    -- console.
    --
    -- This value is null if the configuration doesn\'t invoke an AWS Lambda
    -- function (RecommendationTransformerUri) to perform additional processing
    -- of recommendation data.
    recommendationsDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The type of Amazon Pinpoint ID that\'s associated with unique user IDs
    -- in the recommender model. This value enables the model to use attribute
    -- and event data that’s specific to a particular endpoint or user in an
    -- Amazon Pinpoint application. Possible values are:
    --
    -- -   PINPOINT_ENDPOINT_ID - Each user in the model is associated with a
    --     particular endpoint in Amazon Pinpoint. The data is correlated based
    --     on endpoint IDs in Amazon Pinpoint. This is the default value.
    --
    -- -   PINPOINT_USER_ID - Each user in the model is associated with a
    --     particular user and endpoint in Amazon Pinpoint. The data is
    --     correlated based on user IDs in Amazon Pinpoint. If this value is
    --     specified, an endpoint definition in Amazon Pinpoint has to specify
    --     both a user ID (UserId) and an endpoint ID. Otherwise, messages
    --     won’t be sent to the user\'s endpoint.
    recommendationProviderIdType :: Prelude.Maybe Prelude.Text,
    -- | A map that defines 1-10 custom endpoint or user attributes, depending on
    -- the value for the RecommendationProviderIdType property. Each of these
    -- attributes temporarily stores a recommended item that\'s retrieved from
    -- the recommender model and sent to an AWS Lambda function for additional
    -- processing. Each attribute can be used as a message variable in a
    -- message template.
    --
    -- This value is null if the configuration doesn\'t invoke an AWS Lambda
    -- function (RecommendationTransformerUri) to perform additional processing
    -- of recommendation data.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of recommended items that are retrieved from the model for
    -- each endpoint or user, depending on the value for the
    -- RecommendationProviderIdType property. This number determines how many
    -- recommended items are available for use in message variables.
    recommendationsPerMessage :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the recommender model that Amazon
    -- Pinpoint retrieves the recommendation data from. This value is the ARN
    -- of an Amazon Personalize campaign.
    recommendationProviderUri :: Prelude.Text,
    -- | The date, in extended ISO 8601 format, when the configuration for the
    -- recommender model was last modified.
    lastModifiedDate :: Prelude.Text,
    -- | The date, in extended ISO 8601 format, when the configuration was
    -- created for the recommender model.
    creationDate :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
    -- data from the recommender model.
    recommendationProviderRoleArn :: Prelude.Text,
    -- | The unique identifier for the recommender model configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommenderConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'recommenderConfigurationResponse_name' - The custom name of the configuration for the recommender model.
--
-- 'recommendationTransformerUri', 'recommenderConfigurationResponse_recommendationTransformerUri' - The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to perform additional processing of
-- recommendation data that it retrieves from the recommender model.
--
-- 'description', 'recommenderConfigurationResponse_description' - The custom description of the configuration for the recommender model.
--
-- 'recommendationsDisplayName', 'recommenderConfigurationResponse_recommendationsDisplayName' - The custom display name for the standard endpoint or user attribute
-- (RecommendationItems) that temporarily stores recommended items for each
-- endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This name appears in the
-- __Attribute finder__ of the template editor on the Amazon Pinpoint
-- console.
--
-- This value is null if the configuration doesn\'t invoke an AWS Lambda
-- function (RecommendationTransformerUri) to perform additional processing
-- of recommendation data.
--
-- 'recommendationProviderIdType', 'recommenderConfigurationResponse_recommendationProviderIdType' - The type of Amazon Pinpoint ID that\'s associated with unique user IDs
-- in the recommender model. This value enables the model to use attribute
-- and event data that’s specific to a particular endpoint or user in an
-- Amazon Pinpoint application. Possible values are:
--
-- -   PINPOINT_ENDPOINT_ID - Each user in the model is associated with a
--     particular endpoint in Amazon Pinpoint. The data is correlated based
--     on endpoint IDs in Amazon Pinpoint. This is the default value.
--
-- -   PINPOINT_USER_ID - Each user in the model is associated with a
--     particular user and endpoint in Amazon Pinpoint. The data is
--     correlated based on user IDs in Amazon Pinpoint. If this value is
--     specified, an endpoint definition in Amazon Pinpoint has to specify
--     both a user ID (UserId) and an endpoint ID. Otherwise, messages
--     won’t be sent to the user\'s endpoint.
--
-- 'attributes', 'recommenderConfigurationResponse_attributes' - A map that defines 1-10 custom endpoint or user attributes, depending on
-- the value for the RecommendationProviderIdType property. Each of these
-- attributes temporarily stores a recommended item that\'s retrieved from
-- the recommender model and sent to an AWS Lambda function for additional
-- processing. Each attribute can be used as a message variable in a
-- message template.
--
-- This value is null if the configuration doesn\'t invoke an AWS Lambda
-- function (RecommendationTransformerUri) to perform additional processing
-- of recommendation data.
--
-- 'recommendationsPerMessage', 'recommenderConfigurationResponse_recommendationsPerMessage' - The number of recommended items that are retrieved from the model for
-- each endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This number determines how many
-- recommended items are available for use in message variables.
--
-- 'recommendationProviderUri', 'recommenderConfigurationResponse_recommendationProviderUri' - The Amazon Resource Name (ARN) of the recommender model that Amazon
-- Pinpoint retrieves the recommendation data from. This value is the ARN
-- of an Amazon Personalize campaign.
--
-- 'lastModifiedDate', 'recommenderConfigurationResponse_lastModifiedDate' - The date, in extended ISO 8601 format, when the configuration for the
-- recommender model was last modified.
--
-- 'creationDate', 'recommenderConfigurationResponse_creationDate' - The date, in extended ISO 8601 format, when the configuration was
-- created for the recommender model.
--
-- 'recommendationProviderRoleArn', 'recommenderConfigurationResponse_recommendationProviderRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
--
-- 'id', 'recommenderConfigurationResponse_id' - The unique identifier for the recommender model configuration.
newRecommenderConfigurationResponse ::
  -- | 'recommendationProviderUri'
  Prelude.Text ->
  -- | 'lastModifiedDate'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'recommendationProviderRoleArn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  RecommenderConfigurationResponse
newRecommenderConfigurationResponse
  pRecommendationProviderUri_
  pLastModifiedDate_
  pCreationDate_
  pRecommendationProviderRoleArn_
  pId_ =
    RecommenderConfigurationResponse'
      { name =
          Prelude.Nothing,
        recommendationTransformerUri =
          Prelude.Nothing,
        description = Prelude.Nothing,
        recommendationsDisplayName =
          Prelude.Nothing,
        recommendationProviderIdType =
          Prelude.Nothing,
        attributes = Prelude.Nothing,
        recommendationsPerMessage =
          Prelude.Nothing,
        recommendationProviderUri =
          pRecommendationProviderUri_,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        recommendationProviderRoleArn =
          pRecommendationProviderRoleArn_,
        id = pId_
      }

-- | The custom name of the configuration for the recommender model.
recommenderConfigurationResponse_name :: Lens.Lens' RecommenderConfigurationResponse (Prelude.Maybe Prelude.Text)
recommenderConfigurationResponse_name = Lens.lens (\RecommenderConfigurationResponse' {name} -> name) (\s@RecommenderConfigurationResponse' {} a -> s {name = a} :: RecommenderConfigurationResponse)

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to perform additional processing of
-- recommendation data that it retrieves from the recommender model.
recommenderConfigurationResponse_recommendationTransformerUri :: Lens.Lens' RecommenderConfigurationResponse (Prelude.Maybe Prelude.Text)
recommenderConfigurationResponse_recommendationTransformerUri = Lens.lens (\RecommenderConfigurationResponse' {recommendationTransformerUri} -> recommendationTransformerUri) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationTransformerUri = a} :: RecommenderConfigurationResponse)

-- | The custom description of the configuration for the recommender model.
recommenderConfigurationResponse_description :: Lens.Lens' RecommenderConfigurationResponse (Prelude.Maybe Prelude.Text)
recommenderConfigurationResponse_description = Lens.lens (\RecommenderConfigurationResponse' {description} -> description) (\s@RecommenderConfigurationResponse' {} a -> s {description = a} :: RecommenderConfigurationResponse)

-- | The custom display name for the standard endpoint or user attribute
-- (RecommendationItems) that temporarily stores recommended items for each
-- endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This name appears in the
-- __Attribute finder__ of the template editor on the Amazon Pinpoint
-- console.
--
-- This value is null if the configuration doesn\'t invoke an AWS Lambda
-- function (RecommendationTransformerUri) to perform additional processing
-- of recommendation data.
recommenderConfigurationResponse_recommendationsDisplayName :: Lens.Lens' RecommenderConfigurationResponse (Prelude.Maybe Prelude.Text)
recommenderConfigurationResponse_recommendationsDisplayName = Lens.lens (\RecommenderConfigurationResponse' {recommendationsDisplayName} -> recommendationsDisplayName) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationsDisplayName = a} :: RecommenderConfigurationResponse)

-- | The type of Amazon Pinpoint ID that\'s associated with unique user IDs
-- in the recommender model. This value enables the model to use attribute
-- and event data that’s specific to a particular endpoint or user in an
-- Amazon Pinpoint application. Possible values are:
--
-- -   PINPOINT_ENDPOINT_ID - Each user in the model is associated with a
--     particular endpoint in Amazon Pinpoint. The data is correlated based
--     on endpoint IDs in Amazon Pinpoint. This is the default value.
--
-- -   PINPOINT_USER_ID - Each user in the model is associated with a
--     particular user and endpoint in Amazon Pinpoint. The data is
--     correlated based on user IDs in Amazon Pinpoint. If this value is
--     specified, an endpoint definition in Amazon Pinpoint has to specify
--     both a user ID (UserId) and an endpoint ID. Otherwise, messages
--     won’t be sent to the user\'s endpoint.
recommenderConfigurationResponse_recommendationProviderIdType :: Lens.Lens' RecommenderConfigurationResponse (Prelude.Maybe Prelude.Text)
recommenderConfigurationResponse_recommendationProviderIdType = Lens.lens (\RecommenderConfigurationResponse' {recommendationProviderIdType} -> recommendationProviderIdType) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationProviderIdType = a} :: RecommenderConfigurationResponse)

-- | A map that defines 1-10 custom endpoint or user attributes, depending on
-- the value for the RecommendationProviderIdType property. Each of these
-- attributes temporarily stores a recommended item that\'s retrieved from
-- the recommender model and sent to an AWS Lambda function for additional
-- processing. Each attribute can be used as a message variable in a
-- message template.
--
-- This value is null if the configuration doesn\'t invoke an AWS Lambda
-- function (RecommendationTransformerUri) to perform additional processing
-- of recommendation data.
recommenderConfigurationResponse_attributes :: Lens.Lens' RecommenderConfigurationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recommenderConfigurationResponse_attributes = Lens.lens (\RecommenderConfigurationResponse' {attributes} -> attributes) (\s@RecommenderConfigurationResponse' {} a -> s {attributes = a} :: RecommenderConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of recommended items that are retrieved from the model for
-- each endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This number determines how many
-- recommended items are available for use in message variables.
recommenderConfigurationResponse_recommendationsPerMessage :: Lens.Lens' RecommenderConfigurationResponse (Prelude.Maybe Prelude.Int)
recommenderConfigurationResponse_recommendationsPerMessage = Lens.lens (\RecommenderConfigurationResponse' {recommendationsPerMessage} -> recommendationsPerMessage) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationsPerMessage = a} :: RecommenderConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the recommender model that Amazon
-- Pinpoint retrieves the recommendation data from. This value is the ARN
-- of an Amazon Personalize campaign.
recommenderConfigurationResponse_recommendationProviderUri :: Lens.Lens' RecommenderConfigurationResponse Prelude.Text
recommenderConfigurationResponse_recommendationProviderUri = Lens.lens (\RecommenderConfigurationResponse' {recommendationProviderUri} -> recommendationProviderUri) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationProviderUri = a} :: RecommenderConfigurationResponse)

-- | The date, in extended ISO 8601 format, when the configuration for the
-- recommender model was last modified.
recommenderConfigurationResponse_lastModifiedDate :: Lens.Lens' RecommenderConfigurationResponse Prelude.Text
recommenderConfigurationResponse_lastModifiedDate = Lens.lens (\RecommenderConfigurationResponse' {lastModifiedDate} -> lastModifiedDate) (\s@RecommenderConfigurationResponse' {} a -> s {lastModifiedDate = a} :: RecommenderConfigurationResponse)

-- | The date, in extended ISO 8601 format, when the configuration was
-- created for the recommender model.
recommenderConfigurationResponse_creationDate :: Lens.Lens' RecommenderConfigurationResponse Prelude.Text
recommenderConfigurationResponse_creationDate = Lens.lens (\RecommenderConfigurationResponse' {creationDate} -> creationDate) (\s@RecommenderConfigurationResponse' {} a -> s {creationDate = a} :: RecommenderConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
recommenderConfigurationResponse_recommendationProviderRoleArn :: Lens.Lens' RecommenderConfigurationResponse Prelude.Text
recommenderConfigurationResponse_recommendationProviderRoleArn = Lens.lens (\RecommenderConfigurationResponse' {recommendationProviderRoleArn} -> recommendationProviderRoleArn) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationProviderRoleArn = a} :: RecommenderConfigurationResponse)

-- | The unique identifier for the recommender model configuration.
recommenderConfigurationResponse_id :: Lens.Lens' RecommenderConfigurationResponse Prelude.Text
recommenderConfigurationResponse_id = Lens.lens (\RecommenderConfigurationResponse' {id} -> id) (\s@RecommenderConfigurationResponse' {} a -> s {id = a} :: RecommenderConfigurationResponse)

instance
  Core.FromJSON
    RecommenderConfigurationResponse
  where
  parseJSON =
    Core.withObject
      "RecommenderConfigurationResponse"
      ( \x ->
          RecommenderConfigurationResponse'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RecommendationTransformerUri")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "RecommendationsDisplayName")
            Prelude.<*> (x Core..:? "RecommendationProviderIdType")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RecommendationsPerMessage")
            Prelude.<*> (x Core..: "RecommendationProviderUri")
            Prelude.<*> (x Core..: "LastModifiedDate")
            Prelude.<*> (x Core..: "CreationDate")
            Prelude.<*> (x Core..: "RecommendationProviderRoleArn")
            Prelude.<*> (x Core..: "Id")
      )

instance
  Prelude.Hashable
    RecommenderConfigurationResponse
  where
  hashWithSalt
    _salt
    RecommenderConfigurationResponse' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` recommendationTransformerUri
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` recommendationsDisplayName
        `Prelude.hashWithSalt` recommendationProviderIdType
        `Prelude.hashWithSalt` attributes
        `Prelude.hashWithSalt` recommendationsPerMessage
        `Prelude.hashWithSalt` recommendationProviderUri
        `Prelude.hashWithSalt` lastModifiedDate
        `Prelude.hashWithSalt` creationDate
        `Prelude.hashWithSalt` recommendationProviderRoleArn
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    RecommenderConfigurationResponse
  where
  rnf RecommenderConfigurationResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf recommendationTransformerUri
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf recommendationsDisplayName
      `Prelude.seq` Prelude.rnf recommendationProviderIdType
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf recommendationsPerMessage
      `Prelude.seq` Prelude.rnf recommendationProviderUri
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf recommendationProviderRoleArn
      `Prelude.seq` Prelude.rnf id
