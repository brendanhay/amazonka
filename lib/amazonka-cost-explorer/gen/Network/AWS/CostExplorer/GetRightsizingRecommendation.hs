{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetRightsizingRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates recommendations that help you save cost by identifying idle and underutilized Amazon EC2 instances.
--
--
-- Recommendations are generated to either downsize or terminate instances, along with providing savings detail and metrics. For details on calculation and function, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/ce-rightsizing.html Optimizing Your Cost with Rightsizing Recommendations> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.CostExplorer.GetRightsizingRecommendation
  ( -- * Creating a Request
    getRightsizingRecommendation,
    GetRightsizingRecommendation,

    -- * Request Lenses
    grrNextPageToken,
    grrConfiguration,
    grrFilter,
    grrPageSize,
    grrService,

    -- * Destructuring the Response
    getRightsizingRecommendationResponse,
    GetRightsizingRecommendationResponse,

    -- * Response Lenses
    grrrsSummary,
    grrrsNextPageToken,
    grrrsRightsizingRecommendations,
    grrrsMetadata,
    grrrsConfiguration,
    grrrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRightsizingRecommendation' smart constructor.
data GetRightsizingRecommendation = GetRightsizingRecommendation'
  { _grrNextPageToken ::
      !(Maybe Text),
    _grrConfiguration ::
      !( Maybe
           RightsizingRecommendationConfiguration
       ),
    _grrFilter :: !(Maybe Expression),
    _grrPageSize :: !(Maybe Nat),
    _grrService :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRightsizingRecommendation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrNextPageToken' - The pagination token that indicates the next set of results that you want to retrieve.
--
-- * 'grrConfiguration' - Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
-- * 'grrFilter' - Undocumented member.
--
-- * 'grrPageSize' - The number of recommendations that you want returned in a single response object.
--
-- * 'grrService' - The specific service that you want recommendations for. The only valid value for @GetRightsizingRecommendation@ is "@AmazonEC2@ ".
getRightsizingRecommendation ::
  -- | 'grrService'
  Text ->
  GetRightsizingRecommendation
getRightsizingRecommendation pService_ =
  GetRightsizingRecommendation'
    { _grrNextPageToken = Nothing,
      _grrConfiguration = Nothing,
      _grrFilter = Nothing,
      _grrPageSize = Nothing,
      _grrService = pService_
    }

-- | The pagination token that indicates the next set of results that you want to retrieve.
grrNextPageToken :: Lens' GetRightsizingRecommendation (Maybe Text)
grrNextPageToken = lens _grrNextPageToken (\s a -> s {_grrNextPageToken = a})

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
grrConfiguration :: Lens' GetRightsizingRecommendation (Maybe RightsizingRecommendationConfiguration)
grrConfiguration = lens _grrConfiguration (\s a -> s {_grrConfiguration = a})

-- | Undocumented member.
grrFilter :: Lens' GetRightsizingRecommendation (Maybe Expression)
grrFilter = lens _grrFilter (\s a -> s {_grrFilter = a})

-- | The number of recommendations that you want returned in a single response object.
grrPageSize :: Lens' GetRightsizingRecommendation (Maybe Natural)
grrPageSize = lens _grrPageSize (\s a -> s {_grrPageSize = a}) . mapping _Nat

-- | The specific service that you want recommendations for. The only valid value for @GetRightsizingRecommendation@ is "@AmazonEC2@ ".
grrService :: Lens' GetRightsizingRecommendation Text
grrService = lens _grrService (\s a -> s {_grrService = a})

instance AWSRequest GetRightsizingRecommendation where
  type
    Rs GetRightsizingRecommendation =
      GetRightsizingRecommendationResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          GetRightsizingRecommendationResponse'
            <$> (x .?> "Summary")
            <*> (x .?> "NextPageToken")
            <*> (x .?> "RightsizingRecommendations" .!@ mempty)
            <*> (x .?> "Metadata")
            <*> (x .?> "Configuration")
            <*> (pure (fromEnum s))
      )

instance Hashable GetRightsizingRecommendation

instance NFData GetRightsizingRecommendation

instance ToHeaders GetRightsizingRecommendation where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.GetRightsizingRecommendation" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetRightsizingRecommendation where
  toJSON GetRightsizingRecommendation' {..} =
    object
      ( catMaybes
          [ ("NextPageToken" .=) <$> _grrNextPageToken,
            ("Configuration" .=) <$> _grrConfiguration,
            ("Filter" .=) <$> _grrFilter,
            ("PageSize" .=) <$> _grrPageSize,
            Just ("Service" .= _grrService)
          ]
      )

instance ToPath GetRightsizingRecommendation where
  toPath = const "/"

instance ToQuery GetRightsizingRecommendation where
  toQuery = const mempty

-- | /See:/ 'getRightsizingRecommendationResponse' smart constructor.
data GetRightsizingRecommendationResponse = GetRightsizingRecommendationResponse'
  { _grrrsSummary ::
      !( Maybe
           RightsizingRecommendationSummary
       ),
    _grrrsNextPageToken ::
      !(Maybe Text),
    _grrrsRightsizingRecommendations ::
      !( Maybe
           [RightsizingRecommendation]
       ),
    _grrrsMetadata ::
      !( Maybe
           RightsizingRecommendationMetadata
       ),
    _grrrsConfiguration ::
      !( Maybe
           RightsizingRecommendationConfiguration
       ),
    _grrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRightsizingRecommendationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrrsSummary' - Summary of this recommendation set.
--
-- * 'grrrsNextPageToken' - The token to retrieve the next set of results.
--
-- * 'grrrsRightsizingRecommendations' - Recommendations to rightsize resources.
--
-- * 'grrrsMetadata' - Information regarding this specific recommendation set.
--
-- * 'grrrsConfiguration' - Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
-- * 'grrrsResponseStatus' - -- | The response status code.
getRightsizingRecommendationResponse ::
  -- | 'grrrsResponseStatus'
  Int ->
  GetRightsizingRecommendationResponse
getRightsizingRecommendationResponse pResponseStatus_ =
  GetRightsizingRecommendationResponse'
    { _grrrsSummary = Nothing,
      _grrrsNextPageToken = Nothing,
      _grrrsRightsizingRecommendations = Nothing,
      _grrrsMetadata = Nothing,
      _grrrsConfiguration = Nothing,
      _grrrsResponseStatus = pResponseStatus_
    }

-- | Summary of this recommendation set.
grrrsSummary :: Lens' GetRightsizingRecommendationResponse (Maybe RightsizingRecommendationSummary)
grrrsSummary = lens _grrrsSummary (\s a -> s {_grrrsSummary = a})

-- | The token to retrieve the next set of results.
grrrsNextPageToken :: Lens' GetRightsizingRecommendationResponse (Maybe Text)
grrrsNextPageToken = lens _grrrsNextPageToken (\s a -> s {_grrrsNextPageToken = a})

-- | Recommendations to rightsize resources.
grrrsRightsizingRecommendations :: Lens' GetRightsizingRecommendationResponse [RightsizingRecommendation]
grrrsRightsizingRecommendations = lens _grrrsRightsizingRecommendations (\s a -> s {_grrrsRightsizingRecommendations = a}) . _Default . _Coerce

-- | Information regarding this specific recommendation set.
grrrsMetadata :: Lens' GetRightsizingRecommendationResponse (Maybe RightsizingRecommendationMetadata)
grrrsMetadata = lens _grrrsMetadata (\s a -> s {_grrrsMetadata = a})

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
grrrsConfiguration :: Lens' GetRightsizingRecommendationResponse (Maybe RightsizingRecommendationConfiguration)
grrrsConfiguration = lens _grrrsConfiguration (\s a -> s {_grrrsConfiguration = a})

-- | -- | The response status code.
grrrsResponseStatus :: Lens' GetRightsizingRecommendationResponse Int
grrrsResponseStatus = lens _grrrsResponseStatus (\s a -> s {_grrrsResponseStatus = a})

instance NFData GetRightsizingRecommendationResponse
