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
-- Module      : Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between an approval rule template and one or more specified repositories.
module Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
  ( -- * Creating a Request
    batchDisassociateApprovalRuleTemplateFromRepositories,
    BatchDisassociateApprovalRuleTemplateFromRepositories,

    -- * Request Lenses
    bdartfrApprovalRuleTemplateName,
    bdartfrRepositoryNames,

    -- * Destructuring the Response
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse,
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse,

    -- * Response Lenses
    bdartfrrsResponseStatus,
    bdartfrrsDisassociatedRepositoryNames,
    bdartfrrsErrors,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDisassociateApprovalRuleTemplateFromRepositories' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositories = BatchDisassociateApprovalRuleTemplateFromRepositories'
  { _bdartfrApprovalRuleTemplateName ::
      !Text,
    _bdartfrRepositoryNames ::
      ![Text]
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'BatchDisassociateApprovalRuleTemplateFromRepositories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdartfrApprovalRuleTemplateName' - The name of the template that you want to disassociate from one or more repositories.
--
-- * 'bdartfrRepositoryNames' - The repository names that you want to disassociate from the approval rule template.
batchDisassociateApprovalRuleTemplateFromRepositories ::
  -- | 'bdartfrApprovalRuleTemplateName'
  Text ->
  BatchDisassociateApprovalRuleTemplateFromRepositories
batchDisassociateApprovalRuleTemplateFromRepositories
  pApprovalRuleTemplateName_ =
    BatchDisassociateApprovalRuleTemplateFromRepositories'
      { _bdartfrApprovalRuleTemplateName =
          pApprovalRuleTemplateName_,
        _bdartfrRepositoryNames = mempty
      }

-- | The name of the template that you want to disassociate from one or more repositories.
bdartfrApprovalRuleTemplateName :: Lens' BatchDisassociateApprovalRuleTemplateFromRepositories Text
bdartfrApprovalRuleTemplateName = lens _bdartfrApprovalRuleTemplateName (\s a -> s {_bdartfrApprovalRuleTemplateName = a})

-- | The repository names that you want to disassociate from the approval rule template.
bdartfrRepositoryNames :: Lens' BatchDisassociateApprovalRuleTemplateFromRepositories [Text]
bdartfrRepositoryNames = lens _bdartfrRepositoryNames (\s a -> s {_bdartfrRepositoryNames = a}) . _Coerce

instance
  AWSRequest
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  type
    Rs BatchDisassociateApprovalRuleTemplateFromRepositories =
      BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
            <$> (pure (fromEnum s))
            <*> (x .?> "disassociatedRepositoryNames" .!@ mempty)
            <*> (x .?> "errors" .!@ mempty)
      )

instance
  Hashable
    BatchDisassociateApprovalRuleTemplateFromRepositories

instance
  NFData
    BatchDisassociateApprovalRuleTemplateFromRepositories

instance
  ToHeaders
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.BatchDisassociateApprovalRuleTemplateFromRepositories" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance
  ToJSON
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toJSON BatchDisassociateApprovalRuleTemplateFromRepositories' {..} =
    object
      ( catMaybes
          [ Just
              ("approvalRuleTemplateName" .= _bdartfrApprovalRuleTemplateName),
            Just ("repositoryNames" .= _bdartfrRepositoryNames)
          ]
      )

instance
  ToPath
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toPath = const "/"

instance
  ToQuery
    BatchDisassociateApprovalRuleTemplateFromRepositories
  where
  toQuery = const mempty

-- | /See:/ 'batchDisassociateApprovalRuleTemplateFromRepositoriesResponse' smart constructor.
data BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse = BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
  { _bdartfrrsResponseStatus ::
      !Int,
    _bdartfrrsDisassociatedRepositoryNames ::
      ![Text],
    _bdartfrrsErrors ::
      ![BatchDisassociateApprovalRuleTemplateFromRepositoriesError]
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdartfrrsResponseStatus' - -- | The response status code.
--
-- * 'bdartfrrsDisassociatedRepositoryNames' - A list of repository names that have had their association with the template removed.
--
-- * 'bdartfrrsErrors' - A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
batchDisassociateApprovalRuleTemplateFromRepositoriesResponse ::
  -- | 'bdartfrrsResponseStatus'
  Int ->
  BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
batchDisassociateApprovalRuleTemplateFromRepositoriesResponse
  pResponseStatus_ =
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'
      { _bdartfrrsResponseStatus =
          pResponseStatus_,
        _bdartfrrsDisassociatedRepositoryNames =
          mempty,
        _bdartfrrsErrors = mempty
      }

-- | -- | The response status code.
bdartfrrsResponseStatus :: Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse Int
bdartfrrsResponseStatus = lens _bdartfrrsResponseStatus (\s a -> s {_bdartfrrsResponseStatus = a})

-- | A list of repository names that have had their association with the template removed.
bdartfrrsDisassociatedRepositoryNames :: Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [Text]
bdartfrrsDisassociatedRepositoryNames = lens _bdartfrrsDisassociatedRepositoryNames (\s a -> s {_bdartfrrsDisassociatedRepositoryNames = a}) . _Coerce

-- | A list of any errors that might have occurred while attempting to remove the association between the template and the repositories.
bdartfrrsErrors :: Lens' BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse [BatchDisassociateApprovalRuleTemplateFromRepositoriesError]
bdartfrrsErrors = lens _bdartfrrsErrors (\s a -> s {_bdartfrrsErrors = a}) . _Coerce

instance
  NFData
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
