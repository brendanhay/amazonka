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
-- Module      : Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all approval rule templates that are associated with a specified repository.
module Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
  ( -- * Creating a Request
    listAssociatedApprovalRuleTemplatesForRepository,
    ListAssociatedApprovalRuleTemplatesForRepository,

    -- * Request Lenses
    laartfrNextToken,
    laartfrMaxResults,
    laartfrRepositoryName,

    -- * Destructuring the Response
    listAssociatedApprovalRuleTemplatesForRepositoryResponse,
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse,

    -- * Response Lenses
    laartfrrsNextToken,
    laartfrrsApprovalRuleTemplateNames,
    laartfrrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAssociatedApprovalRuleTemplatesForRepository' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepository = ListAssociatedApprovalRuleTemplatesForRepository'
  { _laartfrNextToken ::
      !( Maybe
           Text
       ),
    _laartfrMaxResults ::
      !( Maybe
           Int
       ),
    _laartfrRepositoryName ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListAssociatedApprovalRuleTemplatesForRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laartfrNextToken' - An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- * 'laartfrMaxResults' - A non-zero, non-negative integer used to limit the number of returned results.
--
-- * 'laartfrRepositoryName' - The name of the repository for which you want to list all associated approval rule templates.
listAssociatedApprovalRuleTemplatesForRepository ::
  -- | 'laartfrRepositoryName'
  Text ->
  ListAssociatedApprovalRuleTemplatesForRepository
listAssociatedApprovalRuleTemplatesForRepository pRepositoryName_ =
  ListAssociatedApprovalRuleTemplatesForRepository'
    { _laartfrNextToken =
        Nothing,
      _laartfrMaxResults = Nothing,
      _laartfrRepositoryName = pRepositoryName_
    }

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
laartfrNextToken :: Lens' ListAssociatedApprovalRuleTemplatesForRepository (Maybe Text)
laartfrNextToken = lens _laartfrNextToken (\s a -> s {_laartfrNextToken = a})

-- | A non-zero, non-negative integer used to limit the number of returned results.
laartfrMaxResults :: Lens' ListAssociatedApprovalRuleTemplatesForRepository (Maybe Int)
laartfrMaxResults = lens _laartfrMaxResults (\s a -> s {_laartfrMaxResults = a})

-- | The name of the repository for which you want to list all associated approval rule templates.
laartfrRepositoryName :: Lens' ListAssociatedApprovalRuleTemplatesForRepository Text
laartfrRepositoryName = lens _laartfrRepositoryName (\s a -> s {_laartfrRepositoryName = a})

instance
  AWSRequest
    ListAssociatedApprovalRuleTemplatesForRepository
  where
  type
    Rs ListAssociatedApprovalRuleTemplatesForRepository =
      ListAssociatedApprovalRuleTemplatesForRepositoryResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "approvalRuleTemplateNames" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListAssociatedApprovalRuleTemplatesForRepository

instance NFData ListAssociatedApprovalRuleTemplatesForRepository

instance ToHeaders ListAssociatedApprovalRuleTemplatesForRepository where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.ListAssociatedApprovalRuleTemplatesForRepository" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListAssociatedApprovalRuleTemplatesForRepository where
  toJSON ListAssociatedApprovalRuleTemplatesForRepository' {..} =
    object
      ( catMaybes
          [ ("nextToken" .=) <$> _laartfrNextToken,
            ("maxResults" .=) <$> _laartfrMaxResults,
            Just ("repositoryName" .= _laartfrRepositoryName)
          ]
      )

instance ToPath ListAssociatedApprovalRuleTemplatesForRepository where
  toPath = const "/"

instance ToQuery ListAssociatedApprovalRuleTemplatesForRepository where
  toQuery = const mempty

-- | /See:/ 'listAssociatedApprovalRuleTemplatesForRepositoryResponse' smart constructor.
data ListAssociatedApprovalRuleTemplatesForRepositoryResponse = ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
  { _laartfrrsNextToken ::
      !( Maybe
           Text
       ),
    _laartfrrsApprovalRuleTemplateNames ::
      !( Maybe
           [Text]
       ),
    _laartfrrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListAssociatedApprovalRuleTemplatesForRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laartfrrsNextToken' - An enumeration token that allows the operation to batch the next results of the operation.
--
-- * 'laartfrrsApprovalRuleTemplateNames' - The names of all approval rule templates associated with the repository.
--
-- * 'laartfrrsResponseStatus' - -- | The response status code.
listAssociatedApprovalRuleTemplatesForRepositoryResponse ::
  -- | 'laartfrrsResponseStatus'
  Int ->
  ListAssociatedApprovalRuleTemplatesForRepositoryResponse
listAssociatedApprovalRuleTemplatesForRepositoryResponse
  pResponseStatus_ =
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse'
      { _laartfrrsNextToken =
          Nothing,
        _laartfrrsApprovalRuleTemplateNames =
          Nothing,
        _laartfrrsResponseStatus =
          pResponseStatus_
      }

-- | An enumeration token that allows the operation to batch the next results of the operation.
laartfrrsNextToken :: Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse (Maybe Text)
laartfrrsNextToken = lens _laartfrrsNextToken (\s a -> s {_laartfrrsNextToken = a})

-- | The names of all approval rule templates associated with the repository.
laartfrrsApprovalRuleTemplateNames :: Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse [Text]
laartfrrsApprovalRuleTemplateNames = lens _laartfrrsApprovalRuleTemplateNames (\s a -> s {_laartfrrsApprovalRuleTemplateNames = a}) . _Default . _Coerce

-- | -- | The response status code.
laartfrrsResponseStatus :: Lens' ListAssociatedApprovalRuleTemplatesForRepositoryResponse Int
laartfrrsResponseStatus = lens _laartfrrsResponseStatus (\s a -> s {_laartfrrsResponseStatus = a})

instance
  NFData
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse
