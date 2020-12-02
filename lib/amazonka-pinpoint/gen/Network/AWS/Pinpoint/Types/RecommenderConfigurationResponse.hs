{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RecommenderConfigurationResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about Amazon Pinpoint configuration settings for retrieving and processing data from a recommender model.
--
--
--
-- /See:/ 'recommenderConfigurationResponse' smart constructor.
data RecommenderConfigurationResponse = RecommenderConfigurationResponse'
  { _rcRecommendationTransformerURI ::
      !(Maybe Text),
    _rcRecommendationsDisplayName ::
      !(Maybe Text),
    _rcRecommendationProviderIdType ::
      !(Maybe Text),
    _rcAttributes ::
      !( Maybe
           (Map Text (Text))
       ),
    _rcName :: !(Maybe Text),
    _rcDescription ::
      !(Maybe Text),
    _rcRecommendationsPerMessage ::
      !(Maybe Int),
    _rcRecommendationProviderURI ::
      !Text,
    _rcLastModifiedDate ::
      !Text,
    _rcCreationDate :: !Text,
    _rcRecommendationProviderRoleARN ::
      !Text,
    _rcId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecommenderConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRecommendationTransformerURI' - The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to perform additional processing of recommendation data that it retrieves from the recommender model.
--
-- * 'rcRecommendationsDisplayName' - The custom display name for the standard endpoint or user attribute (RecommendationItems) that temporarily stores recommended items for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This name appears in the __Attribute finder__ of the template editor on the Amazon Pinpoint console. This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
--
-- * 'rcRecommendationProviderIdType' - The type of Amazon Pinpoint ID that's associated with unique user IDs in the recommender model. This value enables the model to use attribute and event data that’s specific to a particular endpoint or user in an Amazon Pinpoint application. Possible values are:     * PINPOINT_ENDPOINT_ID - Each user in the model is associated with a particular endpoint in Amazon Pinpoint. The data is correlated based on endpoint IDs in Amazon Pinpoint. This is the default value.     * PINPOINT_USER_ID - Each user in the model is associated with a particular user and endpoint in Amazon Pinpoint. The data is correlated based on user IDs in Amazon Pinpoint. If this value is specified, an endpoint definition in Amazon Pinpoint has to specify both a user ID (UserId) and an endpoint ID. Otherwise, messages won’t be sent to the user's endpoint.
--
-- * 'rcAttributes' - A map that defines 1-10 custom endpoint or user attributes, depending on the value for the RecommendationProviderIdType property. Each of these attributes temporarily stores a recommended item that's retrieved from the recommender model and sent to an AWS Lambda function for additional processing. Each attribute can be used as a message variable in a message template. This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
--
-- * 'rcName' - The custom name of the configuration for the recommender model.
--
-- * 'rcDescription' - The custom description of the configuration for the recommender model.
--
-- * 'rcRecommendationsPerMessage' - The number of recommended items that are retrieved from the model for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This number determines how many recommended items are available for use in message variables.
--
-- * 'rcRecommendationProviderURI' - The Amazon Resource Name (ARN) of the recommender model that Amazon Pinpoint retrieves the recommendation data from. This value is the ARN of an Amazon Personalize campaign.
--
-- * 'rcLastModifiedDate' - The date, in extended ISO 8601 format, when the configuration for the recommender model was last modified.
--
-- * 'rcCreationDate' - The date, in extended ISO 8601 format, when the configuration was created for the recommender model.
--
-- * 'rcRecommendationProviderRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation data from the recommender model.
--
-- * 'rcId' - The unique identifier for the recommender model configuration.
recommenderConfigurationResponse ::
  -- | 'rcRecommendationProviderURI'
  Text ->
  -- | 'rcLastModifiedDate'
  Text ->
  -- | 'rcCreationDate'
  Text ->
  -- | 'rcRecommendationProviderRoleARN'
  Text ->
  -- | 'rcId'
  Text ->
  RecommenderConfigurationResponse
recommenderConfigurationResponse
  pRecommendationProviderURI_
  pLastModifiedDate_
  pCreationDate_
  pRecommendationProviderRoleARN_
  pId_ =
    RecommenderConfigurationResponse'
      { _rcRecommendationTransformerURI =
          Nothing,
        _rcRecommendationsDisplayName = Nothing,
        _rcRecommendationProviderIdType = Nothing,
        _rcAttributes = Nothing,
        _rcName = Nothing,
        _rcDescription = Nothing,
        _rcRecommendationsPerMessage = Nothing,
        _rcRecommendationProviderURI = pRecommendationProviderURI_,
        _rcLastModifiedDate = pLastModifiedDate_,
        _rcCreationDate = pCreationDate_,
        _rcRecommendationProviderRoleARN =
          pRecommendationProviderRoleARN_,
        _rcId = pId_
      }

-- | The name or Amazon Resource Name (ARN) of the AWS Lambda function that Amazon Pinpoint invokes to perform additional processing of recommendation data that it retrieves from the recommender model.
rcRecommendationTransformerURI :: Lens' RecommenderConfigurationResponse (Maybe Text)
rcRecommendationTransformerURI = lens _rcRecommendationTransformerURI (\s a -> s {_rcRecommendationTransformerURI = a})

-- | The custom display name for the standard endpoint or user attribute (RecommendationItems) that temporarily stores recommended items for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This name appears in the __Attribute finder__ of the template editor on the Amazon Pinpoint console. This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
rcRecommendationsDisplayName :: Lens' RecommenderConfigurationResponse (Maybe Text)
rcRecommendationsDisplayName = lens _rcRecommendationsDisplayName (\s a -> s {_rcRecommendationsDisplayName = a})

-- | The type of Amazon Pinpoint ID that's associated with unique user IDs in the recommender model. This value enables the model to use attribute and event data that’s specific to a particular endpoint or user in an Amazon Pinpoint application. Possible values are:     * PINPOINT_ENDPOINT_ID - Each user in the model is associated with a particular endpoint in Amazon Pinpoint. The data is correlated based on endpoint IDs in Amazon Pinpoint. This is the default value.     * PINPOINT_USER_ID - Each user in the model is associated with a particular user and endpoint in Amazon Pinpoint. The data is correlated based on user IDs in Amazon Pinpoint. If this value is specified, an endpoint definition in Amazon Pinpoint has to specify both a user ID (UserId) and an endpoint ID. Otherwise, messages won’t be sent to the user's endpoint.
rcRecommendationProviderIdType :: Lens' RecommenderConfigurationResponse (Maybe Text)
rcRecommendationProviderIdType = lens _rcRecommendationProviderIdType (\s a -> s {_rcRecommendationProviderIdType = a})

-- | A map that defines 1-10 custom endpoint or user attributes, depending on the value for the RecommendationProviderIdType property. Each of these attributes temporarily stores a recommended item that's retrieved from the recommender model and sent to an AWS Lambda function for additional processing. Each attribute can be used as a message variable in a message template. This value is null if the configuration doesn't invoke an AWS Lambda function (RecommendationTransformerUri) to perform additional processing of recommendation data.
rcAttributes :: Lens' RecommenderConfigurationResponse (HashMap Text (Text))
rcAttributes = lens _rcAttributes (\s a -> s {_rcAttributes = a}) . _Default . _Map

-- | The custom name of the configuration for the recommender model.
rcName :: Lens' RecommenderConfigurationResponse (Maybe Text)
rcName = lens _rcName (\s a -> s {_rcName = a})

-- | The custom description of the configuration for the recommender model.
rcDescription :: Lens' RecommenderConfigurationResponse (Maybe Text)
rcDescription = lens _rcDescription (\s a -> s {_rcDescription = a})

-- | The number of recommended items that are retrieved from the model for each endpoint or user, depending on the value for the RecommendationProviderIdType property. This number determines how many recommended items are available for use in message variables.
rcRecommendationsPerMessage :: Lens' RecommenderConfigurationResponse (Maybe Int)
rcRecommendationsPerMessage = lens _rcRecommendationsPerMessage (\s a -> s {_rcRecommendationsPerMessage = a})

-- | The Amazon Resource Name (ARN) of the recommender model that Amazon Pinpoint retrieves the recommendation data from. This value is the ARN of an Amazon Personalize campaign.
rcRecommendationProviderURI :: Lens' RecommenderConfigurationResponse Text
rcRecommendationProviderURI = lens _rcRecommendationProviderURI (\s a -> s {_rcRecommendationProviderURI = a})

-- | The date, in extended ISO 8601 format, when the configuration for the recommender model was last modified.
rcLastModifiedDate :: Lens' RecommenderConfigurationResponse Text
rcLastModifiedDate = lens _rcLastModifiedDate (\s a -> s {_rcLastModifiedDate = a})

-- | The date, in extended ISO 8601 format, when the configuration was created for the recommender model.
rcCreationDate :: Lens' RecommenderConfigurationResponse Text
rcCreationDate = lens _rcCreationDate (\s a -> s {_rcCreationDate = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that authorizes Amazon Pinpoint to retrieve recommendation data from the recommender model.
rcRecommendationProviderRoleARN :: Lens' RecommenderConfigurationResponse Text
rcRecommendationProviderRoleARN = lens _rcRecommendationProviderRoleARN (\s a -> s {_rcRecommendationProviderRoleARN = a})

-- | The unique identifier for the recommender model configuration.
rcId :: Lens' RecommenderConfigurationResponse Text
rcId = lens _rcId (\s a -> s {_rcId = a})

instance FromJSON RecommenderConfigurationResponse where
  parseJSON =
    withObject
      "RecommenderConfigurationResponse"
      ( \x ->
          RecommenderConfigurationResponse'
            <$> (x .:? "RecommendationTransformerUri")
            <*> (x .:? "RecommendationsDisplayName")
            <*> (x .:? "RecommendationProviderIdType")
            <*> (x .:? "Attributes" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "Description")
            <*> (x .:? "RecommendationsPerMessage")
            <*> (x .: "RecommendationProviderUri")
            <*> (x .: "LastModifiedDate")
            <*> (x .: "CreationDate")
            <*> (x .: "RecommendationProviderRoleArn")
            <*> (x .: "Id")
      )

instance Hashable RecommenderConfigurationResponse

instance NFData RecommenderConfigurationResponse
