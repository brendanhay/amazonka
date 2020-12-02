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
-- Module      : Network.AWS.CloudFront.ListOriginRequestPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of origin request policies.
--
--
-- You can optionally apply a filter to return only the managed policies created by AWS, or only the custom policies created in your AWS account.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListOriginRequestPolicies
  ( -- * Creating a Request
    listOriginRequestPolicies,
    ListOriginRequestPolicies,

    -- * Request Lenses
    lorpMarker,
    lorpMaxItems,
    lorpType,

    -- * Destructuring the Response
    listOriginRequestPoliciesResponse,
    ListOriginRequestPoliciesResponse,

    -- * Response Lenses
    lorprsOriginRequestPolicyList,
    lorprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listOriginRequestPolicies' smart constructor.
data ListOriginRequestPolicies = ListOriginRequestPolicies'
  { _lorpMarker ::
      !(Maybe Text),
    _lorpMaxItems :: !(Maybe Text),
    _lorpType ::
      !(Maybe OriginRequestPolicyType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListOriginRequestPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorpMarker' - Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- * 'lorpMaxItems' - The maximum number of origin request policies that you want in the response.
--
-- * 'lorpType' - A filter to return only the specified kinds of origin request policies. Valid values are:     * @managed@ – Returns only the managed policies created by AWS.     * @custom@ – Returns only the custom policies created in your AWS account.
listOriginRequestPolicies ::
  ListOriginRequestPolicies
listOriginRequestPolicies =
  ListOriginRequestPolicies'
    { _lorpMarker = Nothing,
      _lorpMaxItems = Nothing,
      _lorpType = Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
lorpMarker :: Lens' ListOriginRequestPolicies (Maybe Text)
lorpMarker = lens _lorpMarker (\s a -> s {_lorpMarker = a})

-- | The maximum number of origin request policies that you want in the response.
lorpMaxItems :: Lens' ListOriginRequestPolicies (Maybe Text)
lorpMaxItems = lens _lorpMaxItems (\s a -> s {_lorpMaxItems = a})

-- | A filter to return only the specified kinds of origin request policies. Valid values are:     * @managed@ – Returns only the managed policies created by AWS.     * @custom@ – Returns only the custom policies created in your AWS account.
lorpType :: Lens' ListOriginRequestPolicies (Maybe OriginRequestPolicyType)
lorpType = lens _lorpType (\s a -> s {_lorpType = a})

instance AWSRequest ListOriginRequestPolicies where
  type
    Rs ListOriginRequestPolicies =
      ListOriginRequestPoliciesResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          ListOriginRequestPoliciesResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable ListOriginRequestPolicies

instance NFData ListOriginRequestPolicies

instance ToHeaders ListOriginRequestPolicies where
  toHeaders = const mempty

instance ToPath ListOriginRequestPolicies where
  toPath = const "/2020-05-31/origin-request-policy"

instance ToQuery ListOriginRequestPolicies where
  toQuery ListOriginRequestPolicies' {..} =
    mconcat
      [ "Marker" =: _lorpMarker,
        "MaxItems" =: _lorpMaxItems,
        "Type" =: _lorpType
      ]

-- | /See:/ 'listOriginRequestPoliciesResponse' smart constructor.
data ListOriginRequestPoliciesResponse = ListOriginRequestPoliciesResponse'
  { _lorprsOriginRequestPolicyList ::
      !( Maybe
           OriginRequestPolicyList
       ),
    _lorprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListOriginRequestPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lorprsOriginRequestPolicyList' - A list of origin request policies.
--
-- * 'lorprsResponseStatus' - -- | The response status code.
listOriginRequestPoliciesResponse ::
  -- | 'lorprsResponseStatus'
  Int ->
  ListOriginRequestPoliciesResponse
listOriginRequestPoliciesResponse pResponseStatus_ =
  ListOriginRequestPoliciesResponse'
    { _lorprsOriginRequestPolicyList =
        Nothing,
      _lorprsResponseStatus = pResponseStatus_
    }

-- | A list of origin request policies.
lorprsOriginRequestPolicyList :: Lens' ListOriginRequestPoliciesResponse (Maybe OriginRequestPolicyList)
lorprsOriginRequestPolicyList = lens _lorprsOriginRequestPolicyList (\s a -> s {_lorprsOriginRequestPolicyList = a})

-- | -- | The response status code.
lorprsResponseStatus :: Lens' ListOriginRequestPoliciesResponse Int
lorprsResponseStatus = lens _lorprsResponseStatus (\s a -> s {_lorprsResponseStatus = a})

instance NFData ListOriginRequestPoliciesResponse
