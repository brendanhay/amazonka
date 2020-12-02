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
-- Module      : Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between a template and a repository so that approval rules based on the template are not automatically created when pull requests are created in the specified repository. This does not delete any approval rules previously created for pull requests through the template association.
module Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
  ( -- * Creating a Request
    disassociateApprovalRuleTemplateFromRepository,
    DisassociateApprovalRuleTemplateFromRepository,

    -- * Request Lenses
    dartfrApprovalRuleTemplateName,
    dartfrRepositoryName,

    -- * Destructuring the Response
    disassociateApprovalRuleTemplateFromRepositoryResponse,
    DisassociateApprovalRuleTemplateFromRepositoryResponse,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateApprovalRuleTemplateFromRepository' smart constructor.
data DisassociateApprovalRuleTemplateFromRepository = DisassociateApprovalRuleTemplateFromRepository'
  { _dartfrApprovalRuleTemplateName ::
      !Text,
    _dartfrRepositoryName ::
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

-- | Creates a value of 'DisassociateApprovalRuleTemplateFromRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dartfrApprovalRuleTemplateName' - The name of the approval rule template to disassociate from a specified repository.
--
-- * 'dartfrRepositoryName' - The name of the repository you want to disassociate from the template.
disassociateApprovalRuleTemplateFromRepository ::
  -- | 'dartfrApprovalRuleTemplateName'
  Text ->
  -- | 'dartfrRepositoryName'
  Text ->
  DisassociateApprovalRuleTemplateFromRepository
disassociateApprovalRuleTemplateFromRepository
  pApprovalRuleTemplateName_
  pRepositoryName_ =
    DisassociateApprovalRuleTemplateFromRepository'
      { _dartfrApprovalRuleTemplateName =
          pApprovalRuleTemplateName_,
        _dartfrRepositoryName = pRepositoryName_
      }

-- | The name of the approval rule template to disassociate from a specified repository.
dartfrApprovalRuleTemplateName :: Lens' DisassociateApprovalRuleTemplateFromRepository Text
dartfrApprovalRuleTemplateName = lens _dartfrApprovalRuleTemplateName (\s a -> s {_dartfrApprovalRuleTemplateName = a})

-- | The name of the repository you want to disassociate from the template.
dartfrRepositoryName :: Lens' DisassociateApprovalRuleTemplateFromRepository Text
dartfrRepositoryName = lens _dartfrRepositoryName (\s a -> s {_dartfrRepositoryName = a})

instance AWSRequest DisassociateApprovalRuleTemplateFromRepository where
  type
    Rs DisassociateApprovalRuleTemplateFromRepository =
      DisassociateApprovalRuleTemplateFromRepositoryResponse
  request = postJSON codeCommit
  response =
    receiveNull
      DisassociateApprovalRuleTemplateFromRepositoryResponse'

instance Hashable DisassociateApprovalRuleTemplateFromRepository

instance NFData DisassociateApprovalRuleTemplateFromRepository

instance ToHeaders DisassociateApprovalRuleTemplateFromRepository where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.DisassociateApprovalRuleTemplateFromRepository" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisassociateApprovalRuleTemplateFromRepository where
  toJSON DisassociateApprovalRuleTemplateFromRepository' {..} =
    object
      ( catMaybes
          [ Just
              ("approvalRuleTemplateName" .= _dartfrApprovalRuleTemplateName),
            Just ("repositoryName" .= _dartfrRepositoryName)
          ]
      )

instance ToPath DisassociateApprovalRuleTemplateFromRepository where
  toPath = const "/"

instance ToQuery DisassociateApprovalRuleTemplateFromRepository where
  toQuery = const mempty

-- | /See:/ 'disassociateApprovalRuleTemplateFromRepositoryResponse' smart constructor.
data DisassociateApprovalRuleTemplateFromRepositoryResponse = DisassociateApprovalRuleTemplateFromRepositoryResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DisassociateApprovalRuleTemplateFromRepositoryResponse' with the minimum fields required to make a request.
disassociateApprovalRuleTemplateFromRepositoryResponse ::
  DisassociateApprovalRuleTemplateFromRepositoryResponse
disassociateApprovalRuleTemplateFromRepositoryResponse =
  DisassociateApprovalRuleTemplateFromRepositoryResponse'

instance
  NFData
    DisassociateApprovalRuleTemplateFromRepositoryResponse
