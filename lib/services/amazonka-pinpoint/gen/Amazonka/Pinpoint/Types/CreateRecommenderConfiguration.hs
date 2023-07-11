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
-- Module      : Amazonka.Pinpoint.Types.CreateRecommenderConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CreateRecommenderConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies Amazon Pinpoint configuration settings for retrieving and
-- processing recommendation data from a recommender model.
--
-- /See:/ 'newCreateRecommenderConfiguration' smart constructor.
data CreateRecommenderConfiguration = CreateRecommenderConfiguration'
  { -- | A map of key-value pairs that defines 1-10 custom endpoint or user
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
    -- | A custom description of the configuration for the recommender model. The
    -- description can contain up to 128 characters. The characters can be
    -- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
    description :: Prelude.Maybe Prelude.Text,
    -- | A custom name of the configuration for the recommender model. The name
    -- must start with a letter or number and it can contain up to 128
    -- characters. The characters can be letters, numbers, spaces, underscores
    -- (_), or hyphens (-).
    name :: Prelude.Maybe Prelude.Text,
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
    -- | The name or Amazon Resource Name (ARN) of the AWS Lambda function to
    -- invoke for additional processing of recommendation data that\'s
    -- retrieved from the recommender model.
    recommendationTransformerUri :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'CreateRecommenderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'description', 'createRecommenderConfiguration_description' - A custom description of the configuration for the recommender model. The
-- description can contain up to 128 characters. The characters can be
-- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
--
-- 'name', 'createRecommenderConfiguration_name' - A custom name of the configuration for the recommender model. The name
-- must start with a letter or number and it can contain up to 128
-- characters. The characters can be letters, numbers, spaces, underscores
-- (_), or hyphens (-).
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
-- 'recommendationTransformerUri', 'createRecommenderConfiguration_recommendationTransformerUri' - The name or Amazon Resource Name (ARN) of the AWS Lambda function to
-- invoke for additional processing of recommendation data that\'s
-- retrieved from the recommender model.
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
-- 'recommendationProviderUri', 'createRecommenderConfiguration_recommendationProviderUri' - The Amazon Resource Name (ARN) of the recommender model to retrieve
-- recommendation data from. This value must match the ARN of an Amazon
-- Personalize campaign.
--
-- 'recommendationProviderRoleArn', 'createRecommenderConfiguration_recommendationProviderRoleArn' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
newCreateRecommenderConfiguration ::
  -- | 'recommendationProviderUri'
  Prelude.Text ->
  -- | 'recommendationProviderRoleArn'
  Prelude.Text ->
  CreateRecommenderConfiguration
newCreateRecommenderConfiguration
  pRecommendationProviderUri_
  pRecommendationProviderRoleArn_ =
    CreateRecommenderConfiguration'
      { attributes =
          Prelude.Nothing,
        description = Prelude.Nothing,
        name = Prelude.Nothing,
        recommendationProviderIdType =
          Prelude.Nothing,
        recommendationTransformerUri =
          Prelude.Nothing,
        recommendationsDisplayName =
          Prelude.Nothing,
        recommendationsPerMessage = Prelude.Nothing,
        recommendationProviderUri =
          pRecommendationProviderUri_,
        recommendationProviderRoleArn =
          pRecommendationProviderRoleArn_
      }

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
createRecommenderConfiguration_attributes :: Lens.Lens' CreateRecommenderConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRecommenderConfiguration_attributes = Lens.lens (\CreateRecommenderConfiguration' {attributes} -> attributes) (\s@CreateRecommenderConfiguration' {} a -> s {attributes = a} :: CreateRecommenderConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A custom description of the configuration for the recommender model. The
-- description can contain up to 128 characters. The characters can be
-- letters, numbers, spaces, or the following symbols: _ ; () , ‐.
createRecommenderConfiguration_description :: Lens.Lens' CreateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
createRecommenderConfiguration_description = Lens.lens (\CreateRecommenderConfiguration' {description} -> description) (\s@CreateRecommenderConfiguration' {} a -> s {description = a} :: CreateRecommenderConfiguration)

-- | A custom name of the configuration for the recommender model. The name
-- must start with a letter or number and it can contain up to 128
-- characters. The characters can be letters, numbers, spaces, underscores
-- (_), or hyphens (-).
createRecommenderConfiguration_name :: Lens.Lens' CreateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
createRecommenderConfiguration_name = Lens.lens (\CreateRecommenderConfiguration' {name} -> name) (\s@CreateRecommenderConfiguration' {} a -> s {name = a} :: CreateRecommenderConfiguration)

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
createRecommenderConfiguration_recommendationProviderIdType :: Lens.Lens' CreateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
createRecommenderConfiguration_recommendationProviderIdType = Lens.lens (\CreateRecommenderConfiguration' {recommendationProviderIdType} -> recommendationProviderIdType) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationProviderIdType = a} :: CreateRecommenderConfiguration)

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function to
-- invoke for additional processing of recommendation data that\'s
-- retrieved from the recommender model.
createRecommenderConfiguration_recommendationTransformerUri :: Lens.Lens' CreateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
createRecommenderConfiguration_recommendationTransformerUri = Lens.lens (\CreateRecommenderConfiguration' {recommendationTransformerUri} -> recommendationTransformerUri) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationTransformerUri = a} :: CreateRecommenderConfiguration)

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
createRecommenderConfiguration_recommendationsDisplayName :: Lens.Lens' CreateRecommenderConfiguration (Prelude.Maybe Prelude.Text)
createRecommenderConfiguration_recommendationsDisplayName = Lens.lens (\CreateRecommenderConfiguration' {recommendationsDisplayName} -> recommendationsDisplayName) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationsDisplayName = a} :: CreateRecommenderConfiguration)

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
createRecommenderConfiguration_recommendationsPerMessage :: Lens.Lens' CreateRecommenderConfiguration (Prelude.Maybe Prelude.Int)
createRecommenderConfiguration_recommendationsPerMessage = Lens.lens (\CreateRecommenderConfiguration' {recommendationsPerMessage} -> recommendationsPerMessage) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationsPerMessage = a} :: CreateRecommenderConfiguration)

-- | The Amazon Resource Name (ARN) of the recommender model to retrieve
-- recommendation data from. This value must match the ARN of an Amazon
-- Personalize campaign.
createRecommenderConfiguration_recommendationProviderUri :: Lens.Lens' CreateRecommenderConfiguration Prelude.Text
createRecommenderConfiguration_recommendationProviderUri = Lens.lens (\CreateRecommenderConfiguration' {recommendationProviderUri} -> recommendationProviderUri) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationProviderUri = a} :: CreateRecommenderConfiguration)

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management
-- (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation
-- data from the recommender model.
createRecommenderConfiguration_recommendationProviderRoleArn :: Lens.Lens' CreateRecommenderConfiguration Prelude.Text
createRecommenderConfiguration_recommendationProviderRoleArn = Lens.lens (\CreateRecommenderConfiguration' {recommendationProviderRoleArn} -> recommendationProviderRoleArn) (\s@CreateRecommenderConfiguration' {} a -> s {recommendationProviderRoleArn = a} :: CreateRecommenderConfiguration)

instance
  Prelude.Hashable
    CreateRecommenderConfiguration
  where
  hashWithSalt
    _salt
    CreateRecommenderConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` attributes
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` recommendationProviderIdType
        `Prelude.hashWithSalt` recommendationTransformerUri
        `Prelude.hashWithSalt` recommendationsDisplayName
        `Prelude.hashWithSalt` recommendationsPerMessage
        `Prelude.hashWithSalt` recommendationProviderUri
        `Prelude.hashWithSalt` recommendationProviderRoleArn

instance
  Prelude.NFData
    CreateRecommenderConfiguration
  where
  rnf CreateRecommenderConfiguration' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recommendationProviderIdType
      `Prelude.seq` Prelude.rnf recommendationTransformerUri
      `Prelude.seq` Prelude.rnf recommendationsDisplayName
      `Prelude.seq` Prelude.rnf recommendationsPerMessage
      `Prelude.seq` Prelude.rnf recommendationProviderUri
      `Prelude.seq` Prelude.rnf recommendationProviderRoleArn

instance Data.ToJSON CreateRecommenderConfiguration where
  toJSON CreateRecommenderConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attributes" Data..=) Prelude.<$> attributes,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("RecommendationProviderIdType" Data..=)
              Prelude.<$> recommendationProviderIdType,
            ("RecommendationTransformerUri" Data..=)
              Prelude.<$> recommendationTransformerUri,
            ("RecommendationsDisplayName" Data..=)
              Prelude.<$> recommendationsDisplayName,
            ("RecommendationsPerMessage" Data..=)
              Prelude.<$> recommendationsPerMessage,
            Prelude.Just
              ( "RecommendationProviderUri"
                  Data..= recommendationProviderUri
              ),
            Prelude.Just
              ( "RecommendationProviderRoleArn"
                  Data..= recommendationProviderRoleArn
              )
          ]
      )
