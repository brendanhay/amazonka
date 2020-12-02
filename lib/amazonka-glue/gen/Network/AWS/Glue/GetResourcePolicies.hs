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
-- Module      : Network.AWS.Glue.GetResourcePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the security configurations for the resource policies set on individual resources, and also the account-level policy.
--
--
-- This operation also returns the Data Catalog resource policy. However, if you enabled metadata encryption in Data Catalog settings, and you do not have permission on the AWS KMS key, the operation can't return the Data Catalog resource policy.
--
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetResourcePolicies
  ( -- * Creating a Request
    getResourcePolicies,
    GetResourcePolicies,

    -- * Request Lenses
    grpNextToken,
    grpMaxResults,

    -- * Destructuring the Response
    getResourcePoliciesResponse,
    GetResourcePoliciesResponse,

    -- * Response Lenses
    grprsGetResourcePoliciesResponseList,
    grprsNextToken,
    grprsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getResourcePolicies' smart constructor.
data GetResourcePolicies = GetResourcePolicies'
  { _grpNextToken ::
      !(Maybe Text),
    _grpMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcePolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grpNextToken' - A continuation token, if this is a continuation request.
--
-- * 'grpMaxResults' - The maximum size of a list to return.
getResourcePolicies ::
  GetResourcePolicies
getResourcePolicies =
  GetResourcePolicies'
    { _grpNextToken = Nothing,
      _grpMaxResults = Nothing
    }

-- | A continuation token, if this is a continuation request.
grpNextToken :: Lens' GetResourcePolicies (Maybe Text)
grpNextToken = lens _grpNextToken (\s a -> s {_grpNextToken = a})

-- | The maximum size of a list to return.
grpMaxResults :: Lens' GetResourcePolicies (Maybe Natural)
grpMaxResults = lens _grpMaxResults (\s a -> s {_grpMaxResults = a}) . mapping _Nat

instance AWSPager GetResourcePolicies where
  page rq rs
    | stop (rs ^. grprsNextToken) = Nothing
    | stop (rs ^. grprsGetResourcePoliciesResponseList) = Nothing
    | otherwise = Just $ rq & grpNextToken .~ rs ^. grprsNextToken

instance AWSRequest GetResourcePolicies where
  type Rs GetResourcePolicies = GetResourcePoliciesResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetResourcePoliciesResponse'
            <$> (x .?> "GetResourcePoliciesResponseList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetResourcePolicies

instance NFData GetResourcePolicies

instance ToHeaders GetResourcePolicies where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetResourcePolicies" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetResourcePolicies where
  toJSON GetResourcePolicies' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _grpNextToken,
            ("MaxResults" .=) <$> _grpMaxResults
          ]
      )

instance ToPath GetResourcePolicies where
  toPath = const "/"

instance ToQuery GetResourcePolicies where
  toQuery = const mempty

-- | /See:/ 'getResourcePoliciesResponse' smart constructor.
data GetResourcePoliciesResponse = GetResourcePoliciesResponse'
  { _grprsGetResourcePoliciesResponseList ::
      !(Maybe [GluePolicy]),
    _grprsNextToken :: !(Maybe Text),
    _grprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcePoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprsGetResourcePoliciesResponseList' - A list of the individual resource policies and the account-level resource policy.
--
-- * 'grprsNextToken' - A continuation token, if the returned list does not contain the last resource policy available.
--
-- * 'grprsResponseStatus' - -- | The response status code.
getResourcePoliciesResponse ::
  -- | 'grprsResponseStatus'
  Int ->
  GetResourcePoliciesResponse
getResourcePoliciesResponse pResponseStatus_ =
  GetResourcePoliciesResponse'
    { _grprsGetResourcePoliciesResponseList =
        Nothing,
      _grprsNextToken = Nothing,
      _grprsResponseStatus = pResponseStatus_
    }

-- | A list of the individual resource policies and the account-level resource policy.
grprsGetResourcePoliciesResponseList :: Lens' GetResourcePoliciesResponse [GluePolicy]
grprsGetResourcePoliciesResponseList = lens _grprsGetResourcePoliciesResponseList (\s a -> s {_grprsGetResourcePoliciesResponseList = a}) . _Default . _Coerce

-- | A continuation token, if the returned list does not contain the last resource policy available.
grprsNextToken :: Lens' GetResourcePoliciesResponse (Maybe Text)
grprsNextToken = lens _grprsNextToken (\s a -> s {_grprsNextToken = a})

-- | -- | The response status code.
grprsResponseStatus :: Lens' GetResourcePoliciesResponse Int
grprsResponseStatus = lens _grprsResponseStatus (\s a -> s {_grprsResponseStatus = a})

instance NFData GetResourcePoliciesResponse
