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
-- Module      : Amazonka.Pinpoint.Types.UpdateRecommenderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.UpdateRecommenderConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies Amazon Pinpoint configuration settings for retrieving and
-- processing recommendation data from a recommender model.
--
-- /See:/ 'newUpdateRecommenderConfiguration' smart constructor.
data UpdateRecommenderConfiguration = UpdateRecommenderConfiguration'
  { -- | A custom name of the configuration for the recommender model. The name
    -- must start with a letter or number and it can contain up to 128
    -- characters. The characters can be letters, numbers, spaces, underscores
    -- (_), or hyphens (-).
    name :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the AWS Lambda function to
    -- invoke for additional processing of recommendation data that\'s
    -- retrieved from the recommender model.
    recommendationTransformerUri :: Prelude.Maybe Prelude.Text,
    -- | A custom description of the configuration for the recommender model. The
    -- description can contain up to 128 characters. The characters can be
    -- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
    description :: Prelude.Maybe Prelude.Text,
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
    recommendationsDisplayName :: Prelude.Maybe Prelude.Text,
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
    recommendationProviderIdType :: Prelude.Maybe Prelude.Text,
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
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    recommendationsPerMessage :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the recommender model to retrieve
    -- recommendation data from. This value must match the ARN of an Amazon
    -- Personalize campaign.
    recommendationProviderUri :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
    -- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
    -- data from the recommender model.
    recommendationProviderRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRecommenderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateRecommenderConfiguration_name' - A custom name of the configuration for the recommender model. The name
-- must start with a letter or number and it can contain up to 128
-- characters. The characters can be letters, numbers, spaces, underscores
-- (_), or hyphens (-).
--
-- 'recommendationTransformerUri', 'updateRecommenderConfiguration_recommendationTransformerUri' - The name or Amazon Resource Name (ARN) of the AWS Lambda function to
-- invoke for additional processing of recommendation data that\'s
-- retrieved from the recommender model.
--
-- 'description', 'updateRecommenderConfiguration_description' - A custom description of the configuration for the recommender model. The
-- description can contain up to 128 characters. The characters can be
-- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
--
-- 'recommendationsDisplayName', 'updateRecommenderConfiguration_recommendationsDisplayName' - A custom display name for the standard endpoint or user attribute
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
-- 'recommendationProviderIdType', 'updateRecommenderConfiguration_recommendationProviderIdType' - The type of Amazon Pinpoint ID to associate with unique user IDs in the
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
-- 'attributes', 'updateRecommenderConfiguration_attributes' - A map of key-value pairs that defines 1-10 custom endpoint or user
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
-- 'recommendationsPerMessage', 'updateRecommenderConfiguration_recommendationsPerMessage' - The number of recommended items to retrieve from the model for each
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
-- 'recommendationProviderUri', 'updateRecommenderConfiguration_recommendationProviderUri' - The Amazon Resource Name (ARN) of the recommender model to retrieve
-- recommendation data from. This value must match the ARN of an Amazon
-- Personalize campaign.
--
-- 'recommendationProviderRoleArn', 'updateRecommenderConfiguration_recommendationProviderRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
newUpdateRecommenderConfiguration ::
  -- | 'recommendationProviderUri'
  Prelude.Text ->
  -- | 'recommendationProviderRoleArn'
  Prelude.Text ->
  UpdateRecommenderConfiguration
newUpdateRecommenderConfiguration
  pRecommendationProviderUri_
  pRecommendationProviderRoleArn_ =
    UpdateRecommenderConfiguration'
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
        recommendationsPerMessage = Prelude.Nothing,
        recommendationProviderUri =
          pRecommendationProviderUri_,
        recommendationProviderRoleArn =
          pRecommendationProviderRoleArn_
      }

-- | A custom name of the configuration for the recommender model. The name
-- must start with a letter or number and it can contain up to 128
-- characters. The characters can be letters, numbers, spaces, underscores
-- (_), or hyphens (-).
updateRecommenderConfiguration_name :: Lens.Lens' UpdateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
updateRecommenderConfiguration_name = Lens.lens (\UpdateRecommenderConfiguration' {name} -> name) (\s@UpdateRecommenderConfiguration' {} a -> s {name = a} :: UpdateRecommenderConfiguration)

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function to
-- invoke for additional processing of recommendation data that\'s
-- retrieved from the recommender model.
updateRecommenderConfiguration_recommendationTransformerUri :: Lens.Lens' UpdateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
updateRecommenderConfiguration_recommendationTransformerUri = Lens.lens (\UpdateRecommenderConfiguration' {recommendationTransformerUri} -> recommendationTransformerUri) (\s@UpdateRecommenderConfiguration' {} a -> s {recommendationTransformerUri = a} :: UpdateRecommenderConfiguration)

-- | A custom description of the configuration for the recommender model. The
-- description can contain up to 128 characters. The characters can be
-- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
updateRecommenderConfiguration_description :: Lens.Lens' UpdateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
updateRecommenderConfiguration_description = Lens.lens (\UpdateRecommenderConfiguration' {description} -> description) (\s@UpdateRecommenderConfiguration' {} a -> s {description = a} :: UpdateRecommenderConfiguration)

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
updateRecommenderConfiguration_recommendationsDisplayName :: Lens.Lens' UpdateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
updateRecommenderConfiguration_recommendationsDisplayName = Lens.lens (\UpdateRecommenderConfiguration' {recommendationsDisplayName} -> recommendationsDisplayName) (\s@UpdateRecommenderConfiguration' {} a -> s {recommendationsDisplayName = a} :: UpdateRecommenderConfiguration)

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
updateRecommenderConfiguration_recommendationProviderIdType :: Lens.Lens' UpdateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
updateRecommenderConfiguration_recommendationProviderIdType = Lens.lens (\UpdateRecommenderConfiguration' {recommendationProviderIdType} -> recommendationProviderIdType) (\s@UpdateRecommenderConfiguration' {} a -> s {recommendationProviderIdType = a} :: UpdateRecommenderConfiguration)

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
updateRecommenderConfiguration_attributes :: Lens.Lens' UpdateRecommenderConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateRecommenderConfiguration_attributes = Lens.lens (\UpdateRecommenderConfiguration' {attributes} -> attributes) (\s@UpdateRecommenderConfiguration' {} a -> s {attributes = a} :: UpdateRecommenderConfiguration) Prelude.. Lens.mapping Lens.coerced

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
updateRecommenderConfiguration_recommendationsPerMessage :: Lens.Lens' UpdateRecommenderConfiguration (Prelude.Maybe Prelude.Int)
updateRecommenderConfiguration_recommendationsPerMessage = Lens.lens (\UpdateRecommenderConfiguration' {recommendationsPerMessage} -> recommendationsPerMessage) (\s@UpdateRecommenderConfiguration' {} a -> s {recommendationsPerMessage = a} :: UpdateRecommenderConfiguration)

-- | The Amazon Resource Name (ARN) of the recommender model to retrieve
-- recommendation data from. This value must match the ARN of an Amazon
-- Personalize campaign.
updateRecommenderConfiguration_recommendationProviderUri :: Lens.Lens' UpdateRecommenderConfiguration Prelude.Text
updateRecommenderConfiguration_recommendationProviderUri = Lens.lens (\UpdateRecommenderConfiguration' {recommendationProviderUri} -> recommendationProviderUri) (\s@UpdateRecommenderConfiguration' {} a -> s {recommendationProviderUri = a} :: UpdateRecommenderConfiguration)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
updateRecommenderConfiguration_recommendationProviderRoleArn :: Lens.Lens' UpdateRecommenderConfiguration Prelude.Text
updateRecommenderConfiguration_recommendationProviderRoleArn = Lens.lens (\UpdateRecommenderConfiguration' {recommendationProviderRoleArn} -> recommendationProviderRoleArn) (\s@UpdateRecommenderConfiguration' {} a -> s {recommendationProviderRoleArn = a} :: UpdateRecommenderConfiguration)

instance
  Prelude.Hashable
    UpdateRecommenderConfiguration
  where
  hashWithSalt
    _salt
    UpdateRecommenderConfiguration' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` recommendationTransformerUri
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` recommendationsDisplayName
        `Prelude.hashWithSalt` recommendationProviderIdType
        `Prelude.hashWithSalt` attributes
        `Prelude.hashWithSalt` recommendationsPerMessage
        `Prelude.hashWithSalt` recommendationProviderUri
        `Prelude.hashWithSalt` recommendationProviderRoleArn

instance
  Prelude.NFData
    UpdateRecommenderConfiguration
  where
  rnf UpdateRecommenderConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf recommendationTransformerUri
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf recommendationsDisplayName
      `Prelude.seq` Prelude.rnf recommendationProviderIdType
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf recommendationsPerMessage
      `Prelude.seq` Prelude.rnf recommendationProviderUri
      `Prelude.seq` Prelude.rnf recommendationProviderRoleArn

instance Core.ToJSON UpdateRecommenderConfiguration where
  toJSON UpdateRecommenderConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("RecommendationTransformerUri" Core..=)
              Prelude.<$> recommendationTransformerUri,
            ("Description" Core..=) Prelude.<$> description,
            ("RecommendationsDisplayName" Core..=)
              Prelude.<$> recommendationsDisplayName,
            ("RecommendationProviderIdType" Core..=)
              Prelude.<$> recommendationProviderIdType,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("RecommendationsPerMessage" Core..=)
              Prelude.<$> recommendationsPerMessage,
            Prelude.Just
              ( "RecommendationProviderUri"
                  Core..= recommendationProviderUri
              ),
            Prelude.Just
              ( "RecommendationProviderRoleArn"
                  Core..= recommendationProviderRoleArn
              )
          ]
      )
