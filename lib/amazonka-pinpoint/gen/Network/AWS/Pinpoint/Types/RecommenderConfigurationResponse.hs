{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
  ( RecommenderConfigurationResponse (..)
  -- * Smart constructor
  , mkRecommenderConfigurationResponse
  -- * Lenses
  , rcrRecommendationProviderUri
  , rcrLastModifiedDate
  , rcrCreationDate
  , rcrRecommendationProviderRoleArn
  , rcrId
  , rcrAttributes
  , rcrDescription
  , rcrName
  , rcrRecommendationProviderIdType
  , rcrRecommendationTransformerUri
  , rcrRecommendationsDisplayName
  , rcrRecommendationsPerMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about Amazon Pinpoint configuration settings for retrieving and processing data from a recommender model.
--
-- /See:/ 'mkRecommenderConfigurationResponse' smart constructor.
data RecommenderConfigurationResponse = RecommenderConfigurationResponse'
  { recommendationProviderUri :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the recommender model that Amazon Pinpoint retrieves the recommendation data from. This value is the ARN of an Amazon Personalize campaign.
  , lastModifiedDate :: Core.Text
    -- ^ The date, in extended ISO 8601 format, when the configuration for the recommender model was last modified.
  , creationDate :: Core.Text
    -- ^ The date, in extended ISO 8601 format, when the configuration was created for the recommender model.
  , recommendationProviderRoleArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation data from the recommender model.
  , id :: Core.Text
    -- ^ The unique identifier for the recommender model configuration.
  , attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A map that defines 1-10 custom endpoint or user attributes, depending on the value for the RecommendationProviderIdType property. Each of these attributes temporarily stores a recommended item that's retrieved from the recommender model and sent to an AWS Lambda function for additional processing. Each attribute can be used as a message variable in a message template.
--
-- This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
  , description :: Core.Maybe Core.Text
    -- ^ The custom description of the configuration for the recommender model.
  , name :: Core.Maybe Core.Text
    -- ^ The custom name of the configuration for the recommender model.
  , recommendationProviderIdType :: Core.Maybe Core.Text
    -- ^ The type of Amazon Pinpoint ID that's associated with unique user IDs in the recommender model. This value enables the model to use attribute and event data that’s specific to a particular endpoint or user in an Amazon Pinpoint application. Possible values are:
--
--
--     * PINPOINT_ENDPOINT_ID - Each user in the model is associated with a particular endpoint in Amazon Pinpoint. The data is correlated based on endpoint IDs in Amazon Pinpoint. This is the default value.
--
--
--     * PINPOINT_USER_ID - Each user in the model is associated with a particular user and endpoint in Amazon Pinpoint. The data is correlated based on user IDs in Amazon Pinpoint. If this value is specified, an endpoint definition in Amazon Pinpoint has to specify both a user ID (UserId) and an endpoint ID. Otherwise, messages won’t be sent to the user's endpoint.
--
--
  , recommendationTransformerUri :: Core.Maybe Core.Text
    -- ^ The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to perform additional processing of recommendation data that it retrieves from the recommender model.
  , recommendationsDisplayName :: Core.Maybe Core.Text
    -- ^ The custom display name for the standard endpoint or user attribute (RecommendationItems) that temporarily stores recommended items for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This name appears in the __Attribute finder__ of the template editor on the Amazon Pinpoint console.
--
-- This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
  , recommendationsPerMessage :: Core.Maybe Core.Int
    -- ^ The number of recommended items that are retrieved from the model for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This number determines how many recommended items are available for use in message variables.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecommenderConfigurationResponse' value with any optional fields omitted.
mkRecommenderConfigurationResponse
    :: Core.Text -- ^ 'recommendationProviderUri'
    -> Core.Text -- ^ 'lastModifiedDate'
    -> Core.Text -- ^ 'creationDate'
    -> Core.Text -- ^ 'recommendationProviderRoleArn'
    -> Core.Text -- ^ 'id'
    -> RecommenderConfigurationResponse
mkRecommenderConfigurationResponse recommendationProviderUri
  lastModifiedDate creationDate recommendationProviderRoleArn id
  = RecommenderConfigurationResponse'{recommendationProviderUri,
                                      lastModifiedDate, creationDate, recommendationProviderRoleArn,
                                      id, attributes = Core.Nothing, description = Core.Nothing,
                                      name = Core.Nothing,
                                      recommendationProviderIdType = Core.Nothing,
                                      recommendationTransformerUri = Core.Nothing,
                                      recommendationsDisplayName = Core.Nothing,
                                      recommendationsPerMessage = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the recommender model that Amazon Pinpoint retrieves the recommendation data from. This value is the ARN of an Amazon Personalize campaign.
--
-- /Note:/ Consider using 'recommendationProviderUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrRecommendationProviderUri :: Lens.Lens' RecommenderConfigurationResponse Core.Text
rcrRecommendationProviderUri = Lens.field @"recommendationProviderUri"
{-# INLINEABLE rcrRecommendationProviderUri #-}
{-# DEPRECATED recommendationProviderUri "Use generic-lens or generic-optics with 'recommendationProviderUri' instead"  #-}

-- | The date, in extended ISO 8601 format, when the configuration for the recommender model was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrLastModifiedDate :: Lens.Lens' RecommenderConfigurationResponse Core.Text
rcrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE rcrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The date, in extended ISO 8601 format, when the configuration was created for the recommender model.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrCreationDate :: Lens.Lens' RecommenderConfigurationResponse Core.Text
rcrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE rcrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation data from the recommender model.
--
-- /Note:/ Consider using 'recommendationProviderRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrRecommendationProviderRoleArn :: Lens.Lens' RecommenderConfigurationResponse Core.Text
rcrRecommendationProviderRoleArn = Lens.field @"recommendationProviderRoleArn"
{-# INLINEABLE rcrRecommendationProviderRoleArn #-}
{-# DEPRECATED recommendationProviderRoleArn "Use generic-lens or generic-optics with 'recommendationProviderRoleArn' instead"  #-}

-- | The unique identifier for the recommender model configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrId :: Lens.Lens' RecommenderConfigurationResponse Core.Text
rcrId = Lens.field @"id"
{-# INLINEABLE rcrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A map that defines 1-10 custom endpoint or user attributes, depending on the value for the RecommendationProviderIdType property. Each of these attributes temporarily stores a recommended item that's retrieved from the recommender model and sent to an AWS Lambda function for additional processing. Each attribute can be used as a message variable in a message template.
--
-- This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrAttributes :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
rcrAttributes = Lens.field @"attributes"
{-# INLINEABLE rcrAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The custom description of the configuration for the recommender model.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrDescription :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
rcrDescription = Lens.field @"description"
{-# INLINEABLE rcrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The custom name of the configuration for the recommender model.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrName :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
rcrName = Lens.field @"name"
{-# INLINEABLE rcrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of Amazon Pinpoint ID that's associated with unique user IDs in the recommender model. This value enables the model to use attribute and event data that’s specific to a particular endpoint or user in an Amazon Pinpoint application. Possible values are:
--
--
--     * PINPOINT_ENDPOINT_ID - Each user in the model is associated with a particular endpoint in Amazon Pinpoint. The data is correlated based on endpoint IDs in Amazon Pinpoint. This is the default value.
--
--
--     * PINPOINT_USER_ID - Each user in the model is associated with a particular user and endpoint in Amazon Pinpoint. The data is correlated based on user IDs in Amazon Pinpoint. If this value is specified, an endpoint definition in Amazon Pinpoint has to specify both a user ID (UserId) and an endpoint ID. Otherwise, messages won’t be sent to the user's endpoint.
--
--
--
-- /Note:/ Consider using 'recommendationProviderIdType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrRecommendationProviderIdType :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
rcrRecommendationProviderIdType = Lens.field @"recommendationProviderIdType"
{-# INLINEABLE rcrRecommendationProviderIdType #-}
{-# DEPRECATED recommendationProviderIdType "Use generic-lens or generic-optics with 'recommendationProviderIdType' instead"  #-}

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to perform additional processing of recommendation data that it retrieves from the recommender model.
--
-- /Note:/ Consider using 'recommendationTransformerUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrRecommendationTransformerUri :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
rcrRecommendationTransformerUri = Lens.field @"recommendationTransformerUri"
{-# INLINEABLE rcrRecommendationTransformerUri #-}
{-# DEPRECATED recommendationTransformerUri "Use generic-lens or generic-optics with 'recommendationTransformerUri' instead"  #-}

-- | The custom display name for the standard endpoint or user attribute (RecommendationItems) that temporarily stores recommended items for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This name appears in the __Attribute finder__ of the template editor on the Amazon Pinpoint console.
--
-- This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
--
-- /Note:/ Consider using 'recommendationsDisplayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrRecommendationsDisplayName :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Text)
rcrRecommendationsDisplayName = Lens.field @"recommendationsDisplayName"
{-# INLINEABLE rcrRecommendationsDisplayName #-}
{-# DEPRECATED recommendationsDisplayName "Use generic-lens or generic-optics with 'recommendationsDisplayName' instead"  #-}

-- | The number of recommended items that are retrieved from the model for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This number determines how many recommended items are available for use in message variables.
--
-- /Note:/ Consider using 'recommendationsPerMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrRecommendationsPerMessage :: Lens.Lens' RecommenderConfigurationResponse (Core.Maybe Core.Int)
rcrRecommendationsPerMessage = Lens.field @"recommendationsPerMessage"
{-# INLINEABLE rcrRecommendationsPerMessage #-}
{-# DEPRECATED recommendationsPerMessage "Use generic-lens or generic-optics with 'recommendationsPerMessage' instead"  #-}

instance Core.FromJSON RecommenderConfigurationResponse where
        parseJSON
          = Core.withObject "RecommenderConfigurationResponse" Core.$
              \ x ->
                RecommenderConfigurationResponse' Core.<$>
                  (x Core..: "RecommendationProviderUri") Core.<*>
                    x Core..: "LastModifiedDate"
                    Core.<*> x Core..: "CreationDate"
                    Core.<*> x Core..: "RecommendationProviderRoleArn"
                    Core.<*> x Core..: "Id"
                    Core.<*> x Core..:? "Attributes"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "RecommendationProviderIdType"
                    Core.<*> x Core..:? "RecommendationTransformerUri"
                    Core.<*> x Core..:? "RecommendationsDisplayName"
                    Core.<*> x Core..:? "RecommendationsPerMessage"
