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
-- Module      : Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about Amazon Pinpoint configuration settings for
-- retrieving and processing data from a recommender model.
--
-- /See:/ 'newRecommenderConfigurationResponse' smart constructor.
data RecommenderConfigurationResponse = RecommenderConfigurationResponse'
  { -- | The custom display name for the standard endpoint or user attribute
    -- (RecommendationItems) that temporarily stores recommended items for each
    -- endpoint or user, depending on the value for the
    -- RecommendationProviderIdType property. This name appears in the
    -- __Attribute finder__ of the template editor on the Amazon Pinpoint
    -- console.
    --
    -- This value is null if the configuration doesn\'t invoke an AWS Lambda
    -- function (RecommendationTransformerUri) to perform additional processing
    -- of recommendation data.
    recommendationsDisplayName :: Core.Maybe Core.Text,
    -- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
    -- Amazon Pinpoint invokes to perform additional processing of
    -- recommendation data that it retrieves from the recommender model.
    recommendationTransformerUri :: Core.Maybe Core.Text,
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
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The custom name of the configuration for the recommender model.
    name :: Core.Maybe Core.Text,
    -- | The number of recommended items that are retrieved from the model for
    -- each endpoint or user, depending on the value for the
    -- RecommendationProviderIdType property. This number determines how many
    -- recommended items are available for use in message variables.
    recommendationsPerMessage :: Core.Maybe Core.Int,
    -- | The custom description of the configuration for the recommender model.
    description :: Core.Maybe Core.Text,
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
    recommendationProviderIdType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the recommender model that Amazon
    -- Pinpoint retrieves the recommendation data from. This value is the ARN
    -- of an Amazon Personalize campaign.
    recommendationProviderUri :: Core.Text,
    -- | The date, in extended ISO 8601 format, when the configuration for the
    -- recommender model was last modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in extended ISO 8601 format, when the configuration was
    -- created for the recommender model.
    creationDate :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
    -- data from the recommender model.
    recommendationProviderRoleArn :: Core.Text,
    -- | The unique identifier for the recommender model configuration.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecommenderConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'recommendationTransformerUri', 'recommenderConfigurationResponse_recommendationTransformerUri' - The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to perform additional processing of
-- recommendation data that it retrieves from the recommender model.
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
-- 'name', 'recommenderConfigurationResponse_name' - The custom name of the configuration for the recommender model.
--
-- 'recommendationsPerMessage', 'recommenderConfigurationResponse_recommendationsPerMessage' - The number of recommended items that are retrieved from the model for
-- each endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This number determines how many
-- recommended items are available for use in message variables.
--
-- 'description', 'recommenderConfigurationResponse_description' - The custom description of the configuration for the recommender model.
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
  Core.Text ->
  -- | 'lastModifiedDate'
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'recommendationProviderRoleArn'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  RecommenderConfigurationResponse
newRecommenderConfigurationResponse
  pRecommendationProviderUri_
  pLastModifiedDate_
  pCreationDate_
  pRecommendationProviderRoleArn_
  pId_ =
    RecommenderConfigurationResponse'
      { recommendationsDisplayName =
          Core.Nothing,
        recommendationTransformerUri =
          Core.Nothing,
        attributes = Core.Nothing,
        name = Core.Nothing,
        recommendationsPerMessage = Core.Nothing,
        description = Core.Nothing,
        recommendationProviderIdType =
          Core.Nothing,
        recommendationProviderUri =
          pRecommendationProviderUri_,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        recommendationProviderRoleArn =
          pRecommendationProviderRoleArn_,
        id = pId_
      }

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
recommenderConfigurationResponse_recommendationsDisplayName :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
recommenderConfigurationResponse_recommendationsDisplayName = Lens.lens (\RecommenderConfigurationResponse' {recommendationsDisplayName} -> recommendationsDisplayName) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationsDisplayName = a} :: RecommenderConfigurationResponse)

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that
-- Amazon Pinpoint invokes to perform additional processing of
-- recommendation data that it retrieves from the recommender model.
recommenderConfigurationResponse_recommendationTransformerUri :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
recommenderConfigurationResponse_recommendationTransformerUri = Lens.lens (\RecommenderConfigurationResponse' {recommendationTransformerUri} -> recommendationTransformerUri) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationTransformerUri = a} :: RecommenderConfigurationResponse)

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
recommenderConfigurationResponse_attributes :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
recommenderConfigurationResponse_attributes = Lens.lens (\RecommenderConfigurationResponse' {attributes} -> attributes) (\s@RecommenderConfigurationResponse' {} a -> s {attributes = a} :: RecommenderConfigurationResponse) Core.. Lens.mapping Lens._Coerce

-- | The custom name of the configuration for the recommender model.
recommenderConfigurationResponse_name :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
recommenderConfigurationResponse_name = Lens.lens (\RecommenderConfigurationResponse' {name} -> name) (\s@RecommenderConfigurationResponse' {} a -> s {name = a} :: RecommenderConfigurationResponse)

-- | The number of recommended items that are retrieved from the model for
-- each endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This number determines how many
-- recommended items are available for use in message variables.
recommenderConfigurationResponse_recommendationsPerMessage :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Int)
recommenderConfigurationResponse_recommendationsPerMessage = Lens.lens (\RecommenderConfigurationResponse' {recommendationsPerMessage} -> recommendationsPerMessage) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationsPerMessage = a} :: RecommenderConfigurationResponse)

-- | The custom description of the configuration for the recommender model.
recommenderConfigurationResponse_description :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
recommenderConfigurationResponse_description = Lens.lens (\RecommenderConfigurationResponse' {description} -> description) (\s@RecommenderConfigurationResponse' {} a -> s {description = a} :: RecommenderConfigurationResponse)

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
recommenderConfigurationResponse_recommendationProviderIdType :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
recommenderConfigurationResponse_recommendationProviderIdType = Lens.lens (\RecommenderConfigurationResponse' {recommendationProviderIdType} -> recommendationProviderIdType) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationProviderIdType = a} :: RecommenderConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the recommender model that Amazon
-- Pinpoint retrieves the recommendation data from. This value is the ARN
-- of an Amazon Personalize campaign.
recommenderConfigurationResponse_recommendationProviderUri :: Lens.Lens' RecommenderConfigurationResponse Core.Text
recommenderConfigurationResponse_recommendationProviderUri = Lens.lens (\RecommenderConfigurationResponse' {recommendationProviderUri} -> recommendationProviderUri) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationProviderUri = a} :: RecommenderConfigurationResponse)

-- | The date, in extended ISO 8601 format, when the configuration for the
-- recommender model was last modified.
recommenderConfigurationResponse_lastModifiedDate :: Lens.Lens' RecommenderConfigurationResponse Core.Text
recommenderConfigurationResponse_lastModifiedDate = Lens.lens (\RecommenderConfigurationResponse' {lastModifiedDate} -> lastModifiedDate) (\s@RecommenderConfigurationResponse' {} a -> s {lastModifiedDate = a} :: RecommenderConfigurationResponse)

-- | The date, in extended ISO 8601 format, when the configuration was
-- created for the recommender model.
recommenderConfigurationResponse_creationDate :: Lens.Lens' RecommenderConfigurationResponse Core.Text
recommenderConfigurationResponse_creationDate = Lens.lens (\RecommenderConfigurationResponse' {creationDate} -> creationDate) (\s@RecommenderConfigurationResponse' {} a -> s {creationDate = a} :: RecommenderConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
recommenderConfigurationResponse_recommendationProviderRoleArn :: Lens.Lens' RecommenderConfigurationResponse Core.Text
recommenderConfigurationResponse_recommendationProviderRoleArn = Lens.lens (\RecommenderConfigurationResponse' {recommendationProviderRoleArn} -> recommendationProviderRoleArn) (\s@RecommenderConfigurationResponse' {} a -> s {recommendationProviderRoleArn = a} :: RecommenderConfigurationResponse)

-- | The unique identifier for the recommender model configuration.
recommenderConfigurationResponse_id :: Lens.Lens' RecommenderConfigurationResponse Core.Text
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
            Core.<$> (x Core..:? "RecommendationsDisplayName")
            Core.<*> (x Core..:? "RecommendationTransformerUri")
            Core.<*> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "RecommendationsPerMessage")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "RecommendationProviderIdType")
            Core.<*> (x Core..: "RecommendationProviderUri")
            Core.<*> (x Core..: "LastModifiedDate")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "RecommendationProviderRoleArn")
            Core.<*> (x Core..: "Id")
      )

instance
  Core.Hashable
    RecommenderConfigurationResponse

instance Core.NFData RecommenderConfigurationResponse
