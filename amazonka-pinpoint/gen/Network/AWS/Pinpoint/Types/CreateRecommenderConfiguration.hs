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
-- Module      : Network.AWS.Pinpoint.Types.CreateRecommenderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CreateRecommenderConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies Amazon Pinpoint configuration settings for retrieving and
-- processing recommendation data from a recommender model.
--
-- /See:/ 'newCreateRecommenderConfiguration' smart constructor.
data CreateRecommenderConfiguration = CreateRecommenderConfiguration'
  { -- | A custom display name for the standard endpoint or user attribute
    -- (RecommendationItems) that temporarily stores recommended items for each
    -- endpoint or user, depending on the value for the
    -- RecommendationProviderIdType property. This value is required if the
    -- configuration doesn\'t invoke an AWS Lambda function
    -- (RecommendationTransformerUri) to perform additional processing of
    -- recommendation data.
    --
    -- This name appears in the __Attribute finder__ of the template editor on
    -- the Amazon Pinpoint console. The name can contain up to 25 characters.
    -- The characters can be letters, numbers, spaces, underscores (_), or
    -- hyphens (-). These restrictions don\'t apply to attribute values.
    recommendationsDisplayName :: Core.Maybe Core.Text,
    -- | The name or Amazon Resource Name (ARN) of the AWS Lambda function to
    -- invoke for additional processing of recommendation data that\'s
    -- retrieved from the recommender model.
    recommendationTransformerUri :: Core.Maybe Core.Text,
    -- | A map of key-value pairs that defines 1-10 custom endpoint or user
    -- attributes, depending on the value for the RecommendationProviderIdType
    -- property. Each of these attributes temporarily stores a recommended item
    -- that\'s retrieved from the recommender model and sent to an AWS Lambda
    -- function for additional processing. Each attribute can be used as a
    -- message variable in a message template.
    --
    -- In the map, the key is the name of a custom attribute and the value is a
    -- custom display name for that attribute. The display name appears in the
    -- __Attribute finder__ of the template editor on the Amazon Pinpoint
    -- console. The following restrictions apply to these names:
    --
    -- -   An attribute name must start with a letter or number and it can
    --     contain up to 50 characters. The characters can be letters, numbers,
    --     underscores (_), or hyphens (-). Attribute names are case sensitive
    --     and must be unique.
    --
    -- -   An attribute display name must start with a letter or number and it
    --     can contain up to 25 characters. The characters can be letters,
    --     numbers, spaces, underscores (_), or hyphens (-).
    --
    -- This object is required if the configuration invokes an AWS Lambda
    -- function (RecommendationTransformerUri) to process recommendation data.
    -- Otherwise, don\'t include this object in your request.
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A custom name of the configuration for the recommender model. The name
    -- must start with a letter or number and it can contain up to 128
    -- characters. The characters can be letters, numbers, spaces, underscores
    -- (_), or hyphens (-).
    name :: Core.Maybe Core.Text,
    -- | The number of recommended items to retrieve from the model for each
    -- endpoint or user, depending on the value for the
    -- RecommendationProviderIdType property. This number determines how many
    -- recommended items are available for use in message variables. The
    -- minimum value is 1. The maximum value is 5. The default value is 5.
    --
    -- To use multiple recommended items and custom attributes with message
    -- variables, you have to use an AWS Lambda function
    -- (RecommendationTransformerUri) to perform additional processing of
    -- recommendation data.
    recommendationsPerMessage :: Core.Maybe Core.Int,
    -- | A custom description of the configuration for the recommender model. The
    -- description can contain up to 128 characters. The characters can be
    -- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
    description :: Core.Maybe Core.Text,
    -- | The type of Amazon Pinpoint ID to associate with unique user IDs in the
    -- recommender model. This value enables the model to use attribute and
    -- event data that’s specific to a particular endpoint or user in an Amazon
    -- Pinpoint application. Valid values are:
    --
    -- -   PINPOINT_ENDPOINT_ID - Associate each user in the model with a
    --     particular endpoint in Amazon Pinpoint. The data is correlated based
    --     on endpoint IDs in Amazon Pinpoint. This is the default value.
    --
    -- -   PINPOINT_USER_ID - Associate each user in the model with a
    --     particular user and endpoint in Amazon Pinpoint. The data is
    --     correlated based on user IDs in Amazon Pinpoint. If you specify this
    --     value, an endpoint definition in Amazon Pinpoint has to specify both
    --     a user ID (UserId) and an endpoint ID. Otherwise, messages won’t be
    --     sent to the user\'s endpoint.
    recommendationProviderIdType :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the recommender model to retrieve
    -- recommendation data from. This value must match the ARN of an Amazon
    -- Personalize campaign.
    recommendationProviderUri :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
    -- data from the recommender model.
    recommendationProviderRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRecommenderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationsDisplayName', 'createRecommenderConfiguration_recommendationsDisplayName' - A custom display name for the standard endpoint or user attribute
-- (RecommendationItems) that temporarily stores recommended items for each
-- endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This value is required if the
-- configuration doesn\'t invoke an AWS Lambda function
-- (RecommendationTransformerUri) to perform additional processing of
-- recommendation data.
--
-- This name appears in the __Attribute finder__ of the template editor on
-- the Amazon Pinpoint console. The name can contain up to 25 characters.
-- The characters can be letters, numbers, spaces, underscores (_), or
-- hyphens (-). These restrictions don\'t apply to attribute values.
--
-- 'recommendationTransformerUri', 'createRecommenderConfiguration_recommendationTransformerUri' - The name or Amazon Resource Name (ARN) of the AWS Lambda function to
-- invoke for additional processing of recommendation data that\'s
-- retrieved from the recommender model.
--
-- 'attributes', 'createRecommenderConfiguration_attributes' - A map of key-value pairs that defines 1-10 custom endpoint or user
-- attributes, depending on the value for the RecommendationProviderIdType
-- property. Each of these attributes temporarily stores a recommended item
-- that\'s retrieved from the recommender model and sent to an AWS Lambda
-- function for additional processing. Each attribute can be used as a
-- message variable in a message template.
--
-- In the map, the key is the name of a custom attribute and the value is a
-- custom display name for that attribute. The display name appears in the
-- __Attribute finder__ of the template editor on the Amazon Pinpoint
-- console. The following restrictions apply to these names:
--
-- -   An attribute name must start with a letter or number and it can
--     contain up to 50 characters. The characters can be letters, numbers,
--     underscores (_), or hyphens (-). Attribute names are case sensitive
--     and must be unique.
--
-- -   An attribute display name must start with a letter or number and it
--     can contain up to 25 characters. The characters can be letters,
--     numbers, spaces, underscores (_), or hyphens (-).
--
-- This object is required if the configuration invokes an AWS Lambda
-- function (RecommendationTransformerUri) to process recommendation data.
-- Otherwise, don\'t include this object in your request.
--
-- 'name', 'createRecommenderConfiguration_name' - A custom name of the configuration for the recommender model. The name
-- must start with a letter or number and it can contain up to 128
-- characters. The characters can be letters, numbers, spaces, underscores
-- (_), or hyphens (-).
--
-- 'recommendationsPerMessage', 'createRecommenderConfiguration_recommendationsPerMessage' - The number of recommended items to retrieve from the model for each
-- endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This number determines how many
-- recommended items are available for use in message variables. The
-- minimum value is 1. The maximum value is 5. The default value is 5.
--
-- To use multiple recommended items and custom attributes with message
-- variables, you have to use an AWS Lambda function
-- (RecommendationTransformerUri) to perform additional processing of
-- recommendation data.
--
-- 'description', 'createRecommenderConfiguration_description' - A custom description of the configuration for the recommender model. The
-- description can contain up to 128 characters. The characters can be
-- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
--
-- 'recommendationProviderIdType', 'createRecommenderConfiguration_recommendationProviderIdType' - The type of Amazon Pinpoint ID to associate with unique user IDs in the
-- recommender model. This value enables the model to use attribute and
-- event data that’s specific to a particular endpoint or user in an Amazon
-- Pinpoint application. Valid values are:
--
-- -   PINPOINT_ENDPOINT_ID - Associate each user in the model with a
--     particular endpoint in Amazon Pinpoint. The data is correlated based
--     on endpoint IDs in Amazon Pinpoint. This is the default value.
--
-- -   PINPOINT_USER_ID - Associate each user in the model with a
--     particular user and endpoint in Amazon Pinpoint. The data is
--     correlated based on user IDs in Amazon Pinpoint. If you specify this
--     value, an endpoint definition in Amazon Pinpoint has to specify both
--     a user ID (UserId) and an endpoint ID. Otherwise, messages won’t be
--     sent to the user\'s endpoint.
--
-- 'recommendationProviderUri', 'createRecommenderConfiguration_recommendationProviderUri' - The Amazon Resource Name (ARN) of the recommender model to retrieve
-- recommendation data from. This value must match the ARN of an Amazon
-- Personalize campaign.
--
-- 'recommendationProviderRoleArn', 'createRecommenderConfiguration_recommendationProviderRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
newCreateRecommenderConfiguration ::
  -- | 'recommendationProviderUri'
  Core.Text ->
  -- | 'recommendationProviderRoleArn'
  Core.Text ->
  CreateRecommenderConfiguration
newCreateRecommenderConfiguration
  pRecommendationProviderUri_
  pRecommendationProviderRoleArn_ =
    CreateRecommenderConfiguration'
      { recommendationsDisplayName =
          Core.Nothing,
        recommendationTransformerUri = Core.Nothing,
        attributes = Core.Nothing,
        name = Core.Nothing,
        recommendationsPerMessage = Core.Nothing,
        description = Core.Nothing,
        recommendationProviderIdType = Core.Nothing,
        recommendationProviderUri =
          pRecommendationProviderUri_,
        recommendationProviderRoleArn =
          pRecommendationProviderRoleArn_
      }

-- | A custom display name for the standard endpoint or user attribute
-- (RecommendationItems) that temporarily stores recommended items for each
-- endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This value is required if the
-- configuration doesn\'t invoke an AWS Lambda function
-- (RecommendationTransformerUri) to perform additional processing of
-- recommendation data.
--
-- This name appears in the __Attribute finder__ of the template editor on
-- the Amazon Pinpoint console. The name can contain up to 25 characters.
-- The characters can be letters, numbers, spaces, underscores (_), or
-- hyphens (-). These restrictions don\'t apply to attribute values.
createRecommenderConfiguration_recommendationsDisplayName :: Lens.Lens' CreateRecommenderConfiguration (Core.Maybe Core.Text)
createRecommenderConfiguration_recommendationsDisplayName = Lens.lens (\CreateRecommenderConfiguration' {recommendationsDisplayName} -> recommendationsDisplayName) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationsDisplayName = a} :: CreateRecommenderConfiguration)

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function to
-- invoke for additional processing of recommendation data that\'s
-- retrieved from the recommender model.
createRecommenderConfiguration_recommendationTransformerUri :: Lens.Lens' CreateRecommenderConfiguration (Core.Maybe Core.Text)
createRecommenderConfiguration_recommendationTransformerUri = Lens.lens (\CreateRecommenderConfiguration' {recommendationTransformerUri} -> recommendationTransformerUri) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationTransformerUri = a} :: CreateRecommenderConfiguration)

-- | A map of key-value pairs that defines 1-10 custom endpoint or user
-- attributes, depending on the value for the RecommendationProviderIdType
-- property. Each of these attributes temporarily stores a recommended item
-- that\'s retrieved from the recommender model and sent to an AWS Lambda
-- function for additional processing. Each attribute can be used as a
-- message variable in a message template.
--
-- In the map, the key is the name of a custom attribute and the value is a
-- custom display name for that attribute. The display name appears in the
-- __Attribute finder__ of the template editor on the Amazon Pinpoint
-- console. The following restrictions apply to these names:
--
-- -   An attribute name must start with a letter or number and it can
--     contain up to 50 characters. The characters can be letters, numbers,
--     underscores (_), or hyphens (-). Attribute names are case sensitive
--     and must be unique.
--
-- -   An attribute display name must start with a letter or number and it
--     can contain up to 25 characters. The characters can be letters,
--     numbers, spaces, underscores (_), or hyphens (-).
--
-- This object is required if the configuration invokes an AWS Lambda
-- function (RecommendationTransformerUri) to process recommendation data.
-- Otherwise, don\'t include this object in your request.
createRecommenderConfiguration_attributes :: Lens.Lens' CreateRecommenderConfiguration (Core.Maybe (Core.HashMap Core.Text Core.Text))
createRecommenderConfiguration_attributes = Lens.lens (\CreateRecommenderConfiguration' {attributes} -> attributes) (\s@CreateRecommenderConfiguration' {} a -> s {attributes = a} :: CreateRecommenderConfiguration) Core.. Lens.mapping Lens._Coerce

-- | A custom name of the configuration for the recommender model. The name
-- must start with a letter or number and it can contain up to 128
-- characters. The characters can be letters, numbers, spaces, underscores
-- (_), or hyphens (-).
createRecommenderConfiguration_name :: Lens.Lens' CreateRecommenderConfiguration (Core.Maybe Core.Text)
createRecommenderConfiguration_name = Lens.lens (\CreateRecommenderConfiguration' {name} -> name) (\s@CreateRecommenderConfiguration' {} a -> s {name = a} :: CreateRecommenderConfiguration)

-- | The number of recommended items to retrieve from the model for each
-- endpoint or user, depending on the value for the
-- RecommendationProviderIdType property. This number determines how many
-- recommended items are available for use in message variables. The
-- minimum value is 1. The maximum value is 5. The default value is 5.
--
-- To use multiple recommended items and custom attributes with message
-- variables, you have to use an AWS Lambda function
-- (RecommendationTransformerUri) to perform additional processing of
-- recommendation data.
createRecommenderConfiguration_recommendationsPerMessage :: Lens.Lens' CreateRecommenderConfiguration (Core.Maybe Core.Int)
createRecommenderConfiguration_recommendationsPerMessage = Lens.lens (\CreateRecommenderConfiguration' {recommendationsPerMessage} -> recommendationsPerMessage) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationsPerMessage = a} :: CreateRecommenderConfiguration)

-- | A custom description of the configuration for the recommender model. The
-- description can contain up to 128 characters. The characters can be
-- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
createRecommenderConfiguration_description :: Lens.Lens' CreateRecommenderConfiguration (Core.Maybe Core.Text)
createRecommenderConfiguration_description = Lens.lens (\CreateRecommenderConfiguration' {description} -> description) (\s@CreateRecommenderConfiguration' {} a -> s {description = a} :: CreateRecommenderConfiguration)

-- | The type of Amazon Pinpoint ID to associate with unique user IDs in the
-- recommender model. This value enables the model to use attribute and
-- event data that’s specific to a particular endpoint or user in an Amazon
-- Pinpoint application. Valid values are:
--
-- -   PINPOINT_ENDPOINT_ID - Associate each user in the model with a
--     particular endpoint in Amazon Pinpoint. The data is correlated based
--     on endpoint IDs in Amazon Pinpoint. This is the default value.
--
-- -   PINPOINT_USER_ID - Associate each user in the model with a
--     particular user and endpoint in Amazon Pinpoint. The data is
--     correlated based on user IDs in Amazon Pinpoint. If you specify this
--     value, an endpoint definition in Amazon Pinpoint has to specify both
--     a user ID (UserId) and an endpoint ID. Otherwise, messages won’t be
--     sent to the user\'s endpoint.
createRecommenderConfiguration_recommendationProviderIdType :: Lens.Lens' CreateRecommenderConfiguration (Core.Maybe Core.Text)
createRecommenderConfiguration_recommendationProviderIdType = Lens.lens (\CreateRecommenderConfiguration' {recommendationProviderIdType} -> recommendationProviderIdType) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationProviderIdType = a} :: CreateRecommenderConfiguration)

-- | The Amazon Resource Name (ARN) of the recommender model to retrieve
-- recommendation data from. This value must match the ARN of an Amazon
-- Personalize campaign.
createRecommenderConfiguration_recommendationProviderUri :: Lens.Lens' CreateRecommenderConfiguration Core.Text
createRecommenderConfiguration_recommendationProviderUri = Lens.lens (\CreateRecommenderConfiguration' {recommendationProviderUri} -> recommendationProviderUri) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationProviderUri = a} :: CreateRecommenderConfiguration)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
createRecommenderConfiguration_recommendationProviderRoleArn :: Lens.Lens' CreateRecommenderConfiguration Core.Text
createRecommenderConfiguration_recommendationProviderRoleArn = Lens.lens (\CreateRecommenderConfiguration' {recommendationProviderRoleArn} -> recommendationProviderRoleArn) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationProviderRoleArn = a} :: CreateRecommenderConfiguration)

instance Core.Hashable CreateRecommenderConfiguration

instance Core.NFData CreateRecommenderConfiguration

instance Core.ToJSON CreateRecommenderConfiguration where
  toJSON CreateRecommenderConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RecommendationsDisplayName" Core..=)
              Core.<$> recommendationsDisplayName,
            ("RecommendationTransformerUri" Core..=)
              Core.<$> recommendationTransformerUri,
            ("Attributes" Core..=) Core.<$> attributes,
            ("Name" Core..=) Core.<$> name,
            ("RecommendationsPerMessage" Core..=)
              Core.<$> recommendationsPerMessage,
            ("Description" Core..=) Core.<$> description,
            ("RecommendationProviderIdType" Core..=)
              Core.<$> recommendationProviderIdType,
            Core.Just
              ( "RecommendationProviderUri"
                  Core..= recommendationProviderUri
              ),
            Core.Just
              ( "RecommendationProviderRoleArn"
                  Core..= recommendationProviderRoleArn
              )
          ]
      )
