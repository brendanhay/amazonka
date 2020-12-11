-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
  ( RecommenderConfigurationResponse (..),

    -- * Smart constructor
    mkRecommenderConfigurationResponse,

    -- * Lenses
    rcRecommendationTransformerURI,
    rcRecommendationsDisplayName,
    rcRecommendationProviderIdType,
    rcAttributes,
    rcName,
    rcDescription,
    rcRecommendationsPerMessage,
    rcRecommendationProviderURI,
    rcLastModifiedDate,
    rcCreationDate,
    rcRecommendationProviderRoleARN,
    rcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about Amazon Pinpoint configuration settings for retrieving and processing data from a recommender model.
--
-- /See:/ 'mkRecommenderConfigurationResponse' smart constructor.
data RecommenderConfigurationResponse = RecommenderConfigurationResponse'
  { recommendationTransformerURI ::
      Lude.Maybe Lude.Text,
    recommendationsDisplayName ::
      Lude.Maybe Lude.Text,
    recommendationProviderIdType ::
      Lude.Maybe Lude.Text,
    attributes ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    name ::
      Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe Lude.Text,
    recommendationsPerMessage ::
      Lude.Maybe Lude.Int,
    recommendationProviderURI ::
      Lude.Text,
    lastModifiedDate ::
      Lude.Text,
    creationDate :: Lude.Text,
    recommendationProviderRoleARN ::
      Lude.Text,
    id :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecommenderConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - A map that defines 1-10 custom endpoint or user attributes, depending on the value for the RecommendationProviderIdType property. Each of these attributes temporarily stores a recommended item that's retrieved from the recommender model and sent to an AWS Lambda function for additional processing. Each attribute can be used as a message variable in a message template.
--
-- This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
-- * 'creationDate' - The date, in extended ISO 8601 format, when the configuration was created for the recommender model.
-- * 'description' - The custom description of the configuration for the recommender model.
-- * 'id' - The unique identifier for the recommender model configuration.
-- * 'lastModifiedDate' - The date, in extended ISO 8601 format, when the configuration for the recommender model was last modified.
-- * 'name' - The custom name of the configuration for the recommender model.
-- * 'recommendationProviderIdType' - The type of Amazon Pinpoint ID that's associated with unique user IDs in the recommender model. This value enables the model to use attribute and event data that’s specific to a particular endpoint or user in an Amazon Pinpoint application. Possible values are:
--
--
--     * PINPOINT_ENDPOINT_ID - Each user in the model is associated with a particular endpoint in Amazon Pinpoint. The data is correlated based on endpoint IDs in Amazon Pinpoint. This is the default value.
--
--
--     * PINPOINT_USER_ID - Each user in the model is associated with a particular user and endpoint in Amazon Pinpoint. The data is correlated based on user IDs in Amazon Pinpoint. If this value is specified, an endpoint definition in Amazon Pinpoint has to specify both a user ID (UserId) and an endpoint ID. Otherwise, messages won’t be sent to the user's endpoint.
--
--
-- * 'recommendationProviderRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation data from the recommender model.
-- * 'recommendationProviderURI' - The Amazon Resource Name (ARN) of the recommender model that Amazon Pinpoint retrieves the recommendation data from. This value is the ARN of an Amazon Personalize campaign.
-- * 'recommendationTransformerURI' - The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to perform additional processing of recommendation data that it retrieves from the recommender model.
-- * 'recommendationsDisplayName' - The custom display name for the standard endpoint or user attribute (RecommendationItems) that temporarily stores recommended items for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This name appears in the __Attribute finder__ of the template editor on the Amazon Pinpoint console.
--
-- This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
-- * 'recommendationsPerMessage' - The number of recommended items that are retrieved from the model for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This number determines how many recommended items are available for use in message variables.
mkRecommenderConfigurationResponse ::
  -- | 'recommendationProviderURI'
  Lude.Text ->
  -- | 'lastModifiedDate'
  Lude.Text ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'recommendationProviderRoleARN'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  RecommenderConfigurationResponse
mkRecommenderConfigurationResponse
  pRecommendationProviderURI_
  pLastModifiedDate_
  pCreationDate_
  pRecommendationProviderRoleARN_
  pId_ =
    RecommenderConfigurationResponse'
      { recommendationTransformerURI =
          Lude.Nothing,
        recommendationsDisplayName = Lude.Nothing,
        recommendationProviderIdType = Lude.Nothing,
        attributes = Lude.Nothing,
        name = Lude.Nothing,
        description = Lude.Nothing,
        recommendationsPerMessage = Lude.Nothing,
        recommendationProviderURI = pRecommendationProviderURI_,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        recommendationProviderRoleARN =
          pRecommendationProviderRoleARN_,
        id = pId_
      }

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to perform additional processing of recommendation data that it retrieves from the recommender model.
--
-- /Note:/ Consider using 'recommendationTransformerURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecommendationTransformerURI :: Lens.Lens' RecommenderConfigurationResponse (Lude.Maybe Lude.Text)
rcRecommendationTransformerURI = Lens.lens (recommendationTransformerURI :: RecommenderConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {recommendationTransformerURI = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcRecommendationTransformerURI "Use generic-lens or generic-optics with 'recommendationTransformerURI' instead." #-}

-- | The custom display name for the standard endpoint or user attribute (RecommendationItems) that temporarily stores recommended items for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This name appears in the __Attribute finder__ of the template editor on the Amazon Pinpoint console.
--
-- This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
--
-- /Note:/ Consider using 'recommendationsDisplayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecommendationsDisplayName :: Lens.Lens' RecommenderConfigurationResponse (Lude.Maybe Lude.Text)
rcRecommendationsDisplayName = Lens.lens (recommendationsDisplayName :: RecommenderConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {recommendationsDisplayName = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcRecommendationsDisplayName "Use generic-lens or generic-optics with 'recommendationsDisplayName' instead." #-}

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
rcRecommendationProviderIdType :: Lens.Lens' RecommenderConfigurationResponse (Lude.Maybe Lude.Text)
rcRecommendationProviderIdType = Lens.lens (recommendationProviderIdType :: RecommenderConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {recommendationProviderIdType = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcRecommendationProviderIdType "Use generic-lens or generic-optics with 'recommendationProviderIdType' instead." #-}

-- | A map that defines 1-10 custom endpoint or user attributes, depending on the value for the RecommendationProviderIdType property. Each of these attributes temporarily stores a recommended item that's retrieved from the recommender model and sent to an AWS Lambda function for additional processing. Each attribute can be used as a message variable in a message template.
--
-- This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAttributes :: Lens.Lens' RecommenderConfigurationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rcAttributes = Lens.lens (attributes :: RecommenderConfigurationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The custom name of the configuration for the recommender model.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcName :: Lens.Lens' RecommenderConfigurationResponse (Lude.Maybe Lude.Text)
rcName = Lens.lens (name :: RecommenderConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The custom description of the configuration for the recommender model.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDescription :: Lens.Lens' RecommenderConfigurationResponse (Lude.Maybe Lude.Text)
rcDescription = Lens.lens (description :: RecommenderConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The number of recommended items that are retrieved from the model for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This number determines how many recommended items are available for use in message variables.
--
-- /Note:/ Consider using 'recommendationsPerMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecommendationsPerMessage :: Lens.Lens' RecommenderConfigurationResponse (Lude.Maybe Lude.Int)
rcRecommendationsPerMessage = Lens.lens (recommendationsPerMessage :: RecommenderConfigurationResponse -> Lude.Maybe Lude.Int) (\s a -> s {recommendationsPerMessage = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcRecommendationsPerMessage "Use generic-lens or generic-optics with 'recommendationsPerMessage' instead." #-}

-- | The Amazon Resource Name (ARN) of the recommender model that Amazon Pinpoint retrieves the recommendation data from. This value is the ARN of an Amazon Personalize campaign.
--
-- /Note:/ Consider using 'recommendationProviderURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecommendationProviderURI :: Lens.Lens' RecommenderConfigurationResponse Lude.Text
rcRecommendationProviderURI = Lens.lens (recommendationProviderURI :: RecommenderConfigurationResponse -> Lude.Text) (\s a -> s {recommendationProviderURI = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcRecommendationProviderURI "Use generic-lens or generic-optics with 'recommendationProviderURI' instead." #-}

-- | The date, in extended ISO 8601 format, when the configuration for the recommender model was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcLastModifiedDate :: Lens.Lens' RecommenderConfigurationResponse Lude.Text
rcLastModifiedDate = Lens.lens (lastModifiedDate :: RecommenderConfigurationResponse -> Lude.Text) (\s a -> s {lastModifiedDate = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The date, in extended ISO 8601 format, when the configuration was created for the recommender model.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCreationDate :: Lens.Lens' RecommenderConfigurationResponse Lude.Text
rcCreationDate = Lens.lens (creationDate :: RecommenderConfigurationResponse -> Lude.Text) (\s a -> s {creationDate = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation data from the recommender model.
--
-- /Note:/ Consider using 'recommendationProviderRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRecommendationProviderRoleARN :: Lens.Lens' RecommenderConfigurationResponse Lude.Text
rcRecommendationProviderRoleARN = Lens.lens (recommendationProviderRoleARN :: RecommenderConfigurationResponse -> Lude.Text) (\s a -> s {recommendationProviderRoleARN = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcRecommendationProviderRoleARN "Use generic-lens or generic-optics with 'recommendationProviderRoleARN' instead." #-}

-- | The unique identifier for the recommender model configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcId :: Lens.Lens' RecommenderConfigurationResponse Lude.Text
rcId = Lens.lens (id :: RecommenderConfigurationResponse -> Lude.Text) (\s a -> s {id = a} :: RecommenderConfigurationResponse)
{-# DEPRECATED rcId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON RecommenderConfigurationResponse where
  parseJSON =
    Lude.withObject
      "RecommenderConfigurationResponse"
      ( \x ->
          RecommenderConfigurationResponse'
            Lude.<$> (x Lude..:? "RecommendationTransformerUri")
            Lude.<*> (x Lude..:? "RecommendationsDisplayName")
            Lude.<*> (x Lude..:? "RecommendationProviderIdType")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "RecommendationsPerMessage")
            Lude.<*> (x Lude..: "RecommendationProviderUri")
            Lude.<*> (x Lude..: "LastModifiedDate")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "RecommendationProviderRoleArn")
            Lude.<*> (x Lude..: "Id")
      )
