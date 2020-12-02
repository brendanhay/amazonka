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
-- Module      : Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between an approval rule template and a specified repository. Then, the next time a pull request is created in the repository where the destination reference (if specified) matches the destination reference (branch) for the pull request, an approval rule that matches the template conditions is automatically created for that pull request. If no destination references are specified in the template, an approval rule that matches the template contents is created for all pull requests in that repository.
module Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
  ( -- * Creating a Request
    associateApprovalRuleTemplateWithRepository,
    AssociateApprovalRuleTemplateWithRepository,

    -- * Request Lenses
    aartwrApprovalRuleTemplateName,
    aartwrRepositoryName,

    -- * Destructuring the Response
    associateApprovalRuleTemplateWithRepositoryResponse,
    AssociateApprovalRuleTemplateWithRepositoryResponse,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateApprovalRuleTemplateWithRepository' smart constructor.
data AssociateApprovalRuleTemplateWithRepository = AssociateApprovalRuleTemplateWithRepository'
  { _aartwrApprovalRuleTemplateName ::
      !Text,
    _aartwrRepositoryName ::
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

-- | Creates a value of 'AssociateApprovalRuleTemplateWithRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aartwrApprovalRuleTemplateName' - The name for the approval rule template.
--
-- * 'aartwrRepositoryName' - The name of the repository that you want to associate with the template.
associateApprovalRuleTemplateWithRepository ::
  -- | 'aartwrApprovalRuleTemplateName'
  Text ->
  -- | 'aartwrRepositoryName'
  Text ->
  AssociateApprovalRuleTemplateWithRepository
associateApprovalRuleTemplateWithRepository
  pApprovalRuleTemplateName_
  pRepositoryName_ =
    AssociateApprovalRuleTemplateWithRepository'
      { _aartwrApprovalRuleTemplateName =
          pApprovalRuleTemplateName_,
        _aartwrRepositoryName = pRepositoryName_
      }

-- | The name for the approval rule template.
aartwrApprovalRuleTemplateName :: Lens' AssociateApprovalRuleTemplateWithRepository Text
aartwrApprovalRuleTemplateName = lens _aartwrApprovalRuleTemplateName (\s a -> s {_aartwrApprovalRuleTemplateName = a})

-- | The name of the repository that you want to associate with the template.
aartwrRepositoryName :: Lens' AssociateApprovalRuleTemplateWithRepository Text
aartwrRepositoryName = lens _aartwrRepositoryName (\s a -> s {_aartwrRepositoryName = a})

instance AWSRequest AssociateApprovalRuleTemplateWithRepository where
  type
    Rs AssociateApprovalRuleTemplateWithRepository =
      AssociateApprovalRuleTemplateWithRepositoryResponse
  request = postJSON codeCommit
  response =
    receiveNull AssociateApprovalRuleTemplateWithRepositoryResponse'

instance Hashable AssociateApprovalRuleTemplateWithRepository

instance NFData AssociateApprovalRuleTemplateWithRepository

instance ToHeaders AssociateApprovalRuleTemplateWithRepository where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.AssociateApprovalRuleTemplateWithRepository" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssociateApprovalRuleTemplateWithRepository where
  toJSON AssociateApprovalRuleTemplateWithRepository' {..} =
    object
      ( catMaybes
          [ Just
              ("approvalRuleTemplateName" .= _aartwrApprovalRuleTemplateName),
            Just ("repositoryName" .= _aartwrRepositoryName)
          ]
      )

instance ToPath AssociateApprovalRuleTemplateWithRepository where
  toPath = const "/"

instance ToQuery AssociateApprovalRuleTemplateWithRepository where
  toQuery = const mempty

-- | /See:/ 'associateApprovalRuleTemplateWithRepositoryResponse' smart constructor.
data AssociateApprovalRuleTemplateWithRepositoryResponse = AssociateApprovalRuleTemplateWithRepositoryResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AssociateApprovalRuleTemplateWithRepositoryResponse' with the minimum fields required to make a request.
associateApprovalRuleTemplateWithRepositoryResponse ::
  AssociateApprovalRuleTemplateWithRepositoryResponse
associateApprovalRuleTemplateWithRepositoryResponse =
  AssociateApprovalRuleTemplateWithRepositoryResponse'

instance NFData AssociateApprovalRuleTemplateWithRepositoryResponse
