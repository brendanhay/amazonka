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
-- Module      : Network.AWS.IoT.ListProvisioningTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the fleet provisioning templates in your AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListProvisioningTemplates
  ( -- * Creating a Request
    listProvisioningTemplates,
    ListProvisioningTemplates,

    -- * Request Lenses
    lNextToken,
    lMaxResults,

    -- * Destructuring the Response
    listProvisioningTemplatesResponse,
    ListProvisioningTemplatesResponse,

    -- * Response Lenses
    lrsTemplates,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listProvisioningTemplates' smart constructor.
data ListProvisioningTemplates = ListProvisioningTemplates'
  { _lNextToken ::
      !(Maybe Text),
    _lMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProvisioningTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken' - A token to retrieve the next set of results.
--
-- * 'lMaxResults' - The maximum number of results to return at one time.
listProvisioningTemplates ::
  ListProvisioningTemplates
listProvisioningTemplates =
  ListProvisioningTemplates'
    { _lNextToken = Nothing,
      _lMaxResults = Nothing
    }

-- | A token to retrieve the next set of results.
lNextToken :: Lens' ListProvisioningTemplates (Maybe Text)
lNextToken = lens _lNextToken (\s a -> s {_lNextToken = a})

-- | The maximum number of results to return at one time.
lMaxResults :: Lens' ListProvisioningTemplates (Maybe Natural)
lMaxResults = lens _lMaxResults (\s a -> s {_lMaxResults = a}) . mapping _Nat

instance AWSPager ListProvisioningTemplates where
  page rq rs
    | stop (rs ^. lrsNextToken) = Nothing
    | stop (rs ^. lrsTemplates) = Nothing
    | otherwise = Just $ rq & lNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListProvisioningTemplates where
  type
    Rs ListProvisioningTemplates =
      ListProvisioningTemplatesResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          ListProvisioningTemplatesResponse'
            <$> (x .?> "templates" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListProvisioningTemplates

instance NFData ListProvisioningTemplates

instance ToHeaders ListProvisioningTemplates where
  toHeaders = const mempty

instance ToPath ListProvisioningTemplates where
  toPath = const "/provisioning-templates"

instance ToQuery ListProvisioningTemplates where
  toQuery ListProvisioningTemplates' {..} =
    mconcat
      ["nextToken" =: _lNextToken, "maxResults" =: _lMaxResults]

-- | /See:/ 'listProvisioningTemplatesResponse' smart constructor.
data ListProvisioningTemplatesResponse = ListProvisioningTemplatesResponse'
  { _lrsTemplates ::
      !( Maybe
           [ProvisioningTemplateSummary]
       ),
    _lrsNextToken ::
      !(Maybe Text),
    _lrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListProvisioningTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsTemplates' - A list of fleet provisioning templates
--
-- * 'lrsNextToken' - A token to retrieve the next set of results.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listProvisioningTemplatesResponse ::
  -- | 'lrsResponseStatus'
  Int ->
  ListProvisioningTemplatesResponse
listProvisioningTemplatesResponse pResponseStatus_ =
  ListProvisioningTemplatesResponse'
    { _lrsTemplates = Nothing,
      _lrsNextToken = Nothing,
      _lrsResponseStatus = pResponseStatus_
    }

-- | A list of fleet provisioning templates
lrsTemplates :: Lens' ListProvisioningTemplatesResponse [ProvisioningTemplateSummary]
lrsTemplates = lens _lrsTemplates (\s a -> s {_lrsTemplates = a}) . _Default . _Coerce

-- | A token to retrieve the next set of results.
lrsNextToken :: Lens' ListProvisioningTemplatesResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\s a -> s {_lrsNextToken = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListProvisioningTemplatesResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\s a -> s {_lrsResponseStatus = a})

instance NFData ListProvisioningTemplatesResponse
