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
-- Module      : Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified approval rule template. Deleting a template does not remove approval rules on pull requests already created with the template.
module Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
  ( -- * Creating a Request
    deleteApprovalRuleTemplate,
    DeleteApprovalRuleTemplate,

    -- * Request Lenses
    dartApprovalRuleTemplateName,

    -- * Destructuring the Response
    deleteApprovalRuleTemplateResponse,
    DeleteApprovalRuleTemplateResponse,

    -- * Response Lenses
    dartrsResponseStatus,
    dartrsApprovalRuleTemplateId,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteApprovalRuleTemplate' smart constructor.
newtype DeleteApprovalRuleTemplate = DeleteApprovalRuleTemplate'
  { _dartApprovalRuleTemplateName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dartApprovalRuleTemplateName' - The name of the approval rule template to delete.
deleteApprovalRuleTemplate ::
  -- | 'dartApprovalRuleTemplateName'
  Text ->
  DeleteApprovalRuleTemplate
deleteApprovalRuleTemplate pApprovalRuleTemplateName_ =
  DeleteApprovalRuleTemplate'
    { _dartApprovalRuleTemplateName =
        pApprovalRuleTemplateName_
    }

-- | The name of the approval rule template to delete.
dartApprovalRuleTemplateName :: Lens' DeleteApprovalRuleTemplate Text
dartApprovalRuleTemplateName = lens _dartApprovalRuleTemplateName (\s a -> s {_dartApprovalRuleTemplateName = a})

instance AWSRequest DeleteApprovalRuleTemplate where
  type
    Rs DeleteApprovalRuleTemplate =
      DeleteApprovalRuleTemplateResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          DeleteApprovalRuleTemplateResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "approvalRuleTemplateId")
      )

instance Hashable DeleteApprovalRuleTemplate

instance NFData DeleteApprovalRuleTemplate

instance ToHeaders DeleteApprovalRuleTemplate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.DeleteApprovalRuleTemplate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteApprovalRuleTemplate where
  toJSON DeleteApprovalRuleTemplate' {..} =
    object
      ( catMaybes
          [ Just
              ("approvalRuleTemplateName" .= _dartApprovalRuleTemplateName)
          ]
      )

instance ToPath DeleteApprovalRuleTemplate where
  toPath = const "/"

instance ToQuery DeleteApprovalRuleTemplate where
  toQuery = const mempty

-- | /See:/ 'deleteApprovalRuleTemplateResponse' smart constructor.
data DeleteApprovalRuleTemplateResponse = DeleteApprovalRuleTemplateResponse'
  { _dartrsResponseStatus ::
      !Int,
    _dartrsApprovalRuleTemplateId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteApprovalRuleTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dartrsResponseStatus' - -- | The response status code.
--
-- * 'dartrsApprovalRuleTemplateId' - The system-generated ID of the deleted approval rule template. If the template has been previously deleted, the only response is a 200 OK.
deleteApprovalRuleTemplateResponse ::
  -- | 'dartrsResponseStatus'
  Int ->
  -- | 'dartrsApprovalRuleTemplateId'
  Text ->
  DeleteApprovalRuleTemplateResponse
deleteApprovalRuleTemplateResponse
  pResponseStatus_
  pApprovalRuleTemplateId_ =
    DeleteApprovalRuleTemplateResponse'
      { _dartrsResponseStatus =
          pResponseStatus_,
        _dartrsApprovalRuleTemplateId = pApprovalRuleTemplateId_
      }

-- | -- | The response status code.
dartrsResponseStatus :: Lens' DeleteApprovalRuleTemplateResponse Int
dartrsResponseStatus = lens _dartrsResponseStatus (\s a -> s {_dartrsResponseStatus = a})

-- | The system-generated ID of the deleted approval rule template. If the template has been previously deleted, the only response is a 200 OK.
dartrsApprovalRuleTemplateId :: Lens' DeleteApprovalRuleTemplateResponse Text
dartrsApprovalRuleTemplateId = lens _dartrsApprovalRuleTemplateId (\s a -> s {_dartrsApprovalRuleTemplateId = a})

instance NFData DeleteApprovalRuleTemplateResponse
