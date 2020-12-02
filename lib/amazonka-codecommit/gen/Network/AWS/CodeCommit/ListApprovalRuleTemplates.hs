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
-- Module      : Network.AWS.CodeCommit.ListApprovalRuleTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all approval rule templates in the specified AWS Region in your AWS account. If an AWS Region is not specified, the AWS Region where you are signed in is used.
module Network.AWS.CodeCommit.ListApprovalRuleTemplates
  ( -- * Creating a Request
    listApprovalRuleTemplates,
    ListApprovalRuleTemplates,

    -- * Request Lenses
    lartNextToken,
    lartMaxResults,

    -- * Destructuring the Response
    listApprovalRuleTemplatesResponse,
    ListApprovalRuleTemplatesResponse,

    -- * Response Lenses
    lartrsNextToken,
    lartrsApprovalRuleTemplateNames,
    lartrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listApprovalRuleTemplates' smart constructor.
data ListApprovalRuleTemplates = ListApprovalRuleTemplates'
  { _lartNextToken ::
      !(Maybe Text),
    _lartMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApprovalRuleTemplates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lartNextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- * 'lartMaxResults' - A non-zero, non-negative integer used to limit the number of returned results.
listApprovalRuleTemplates ::
  ListApprovalRuleTemplates
listApprovalRuleTemplates =
  ListApprovalRuleTemplates'
    { _lartNextToken = Nothing,
      _lartMaxResults = Nothing
    }

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
lartNextToken :: Lens' ListApprovalRuleTemplates (Maybe Text)
lartNextToken = lens _lartNextToken (\s a -> s {_lartNextToken = a})

-- | A non-zero, non-negative integer used to limit the number of returned results.
lartMaxResults :: Lens' ListApprovalRuleTemplates (Maybe Int)
lartMaxResults = lens _lartMaxResults (\s a -> s {_lartMaxResults = a})

instance AWSRequest ListApprovalRuleTemplates where
  type
    Rs ListApprovalRuleTemplates =
      ListApprovalRuleTemplatesResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          ListApprovalRuleTemplatesResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "approvalRuleTemplateNames" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListApprovalRuleTemplates

instance NFData ListApprovalRuleTemplates

instance ToHeaders ListApprovalRuleTemplates where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.ListApprovalRuleTemplates" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListApprovalRuleTemplates where
  toJSON ListApprovalRuleTemplates' {..} =
    object
      ( catMaybes
          [ ("nextToken" .=) <$> _lartNextToken,
            ("maxResults" .=) <$> _lartMaxResults
          ]
      )

instance ToPath ListApprovalRuleTemplates where
  toPath = const "/"

instance ToQuery ListApprovalRuleTemplates where
  toQuery = const mempty

-- | /See:/ 'listApprovalRuleTemplatesResponse' smart constructor.
data ListApprovalRuleTemplatesResponse = ListApprovalRuleTemplatesResponse'
  { _lartrsNextToken ::
      !(Maybe Text),
    _lartrsApprovalRuleTemplateNames ::
      !(Maybe [Text]),
    _lartrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApprovalRuleTemplatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lartrsNextToken' - An enumeration token that allows the operation to batch the next results of the operation.
--
-- * 'lartrsApprovalRuleTemplateNames' - The names of all the approval rule templates found in the AWS Region for your AWS account.
--
-- * 'lartrsResponseStatus' - -- | The response status code.
listApprovalRuleTemplatesResponse ::
  -- | 'lartrsResponseStatus'
  Int ->
  ListApprovalRuleTemplatesResponse
listApprovalRuleTemplatesResponse pResponseStatus_ =
  ListApprovalRuleTemplatesResponse'
    { _lartrsNextToken = Nothing,
      _lartrsApprovalRuleTemplateNames = Nothing,
      _lartrsResponseStatus = pResponseStatus_
    }

-- | An enumeration token that allows the operation to batch the next results of the operation.
lartrsNextToken :: Lens' ListApprovalRuleTemplatesResponse (Maybe Text)
lartrsNextToken = lens _lartrsNextToken (\s a -> s {_lartrsNextToken = a})

-- | The names of all the approval rule templates found in the AWS Region for your AWS account.
lartrsApprovalRuleTemplateNames :: Lens' ListApprovalRuleTemplatesResponse [Text]
lartrsApprovalRuleTemplateNames = lens _lartrsApprovalRuleTemplateNames (\s a -> s {_lartrsApprovalRuleTemplateNames = a}) . _Default . _Coerce

-- | -- | The response status code.
lartrsResponseStatus :: Lens' ListApprovalRuleTemplatesResponse Int
lartrsResponseStatus = lens _lartrsResponseStatus (\s a -> s {_lartrsResponseStatus = a})

instance NFData ListApprovalRuleTemplatesResponse
