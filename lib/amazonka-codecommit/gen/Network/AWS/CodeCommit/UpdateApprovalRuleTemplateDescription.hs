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
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description for a specified approval rule template.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
  ( -- * Creating a Request
    updateApprovalRuleTemplateDescription,
    UpdateApprovalRuleTemplateDescription,

    -- * Request Lenses
    uartdApprovalRuleTemplateName,
    uartdApprovalRuleTemplateDescription,

    -- * Destructuring the Response
    updateApprovalRuleTemplateDescriptionResponse,
    UpdateApprovalRuleTemplateDescriptionResponse,

    -- * Response Lenses
    uartdrsResponseStatus,
    uartdrsApprovalRuleTemplate,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateApprovalRuleTemplateDescription' smart constructor.
data UpdateApprovalRuleTemplateDescription = UpdateApprovalRuleTemplateDescription'
  { _uartdApprovalRuleTemplateName ::
      !Text,
    _uartdApprovalRuleTemplateDescription ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateApprovalRuleTemplateDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uartdApprovalRuleTemplateName' - The name of the template for which you want to update the description.
--
-- * 'uartdApprovalRuleTemplateDescription' - The updated description of the approval rule template.
updateApprovalRuleTemplateDescription ::
  -- | 'uartdApprovalRuleTemplateName'
  Text ->
  -- | 'uartdApprovalRuleTemplateDescription'
  Text ->
  UpdateApprovalRuleTemplateDescription
updateApprovalRuleTemplateDescription
  pApprovalRuleTemplateName_
  pApprovalRuleTemplateDescription_ =
    UpdateApprovalRuleTemplateDescription'
      { _uartdApprovalRuleTemplateName =
          pApprovalRuleTemplateName_,
        _uartdApprovalRuleTemplateDescription =
          pApprovalRuleTemplateDescription_
      }

-- | The name of the template for which you want to update the description.
uartdApprovalRuleTemplateName :: Lens' UpdateApprovalRuleTemplateDescription Text
uartdApprovalRuleTemplateName = lens _uartdApprovalRuleTemplateName (\s a -> s {_uartdApprovalRuleTemplateName = a})

-- | The updated description of the approval rule template.
uartdApprovalRuleTemplateDescription :: Lens' UpdateApprovalRuleTemplateDescription Text
uartdApprovalRuleTemplateDescription = lens _uartdApprovalRuleTemplateDescription (\s a -> s {_uartdApprovalRuleTemplateDescription = a})

instance AWSRequest UpdateApprovalRuleTemplateDescription where
  type
    Rs UpdateApprovalRuleTemplateDescription =
      UpdateApprovalRuleTemplateDescriptionResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateDescriptionResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "approvalRuleTemplate")
      )

instance Hashable UpdateApprovalRuleTemplateDescription

instance NFData UpdateApprovalRuleTemplateDescription

instance ToHeaders UpdateApprovalRuleTemplateDescription where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.UpdateApprovalRuleTemplateDescription" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateApprovalRuleTemplateDescription where
  toJSON UpdateApprovalRuleTemplateDescription' {..} =
    object
      ( catMaybes
          [ Just
              ("approvalRuleTemplateName" .= _uartdApprovalRuleTemplateName),
            Just
              ( "approvalRuleTemplateDescription"
                  .= _uartdApprovalRuleTemplateDescription
              )
          ]
      )

instance ToPath UpdateApprovalRuleTemplateDescription where
  toPath = const "/"

instance ToQuery UpdateApprovalRuleTemplateDescription where
  toQuery = const mempty

-- | /See:/ 'updateApprovalRuleTemplateDescriptionResponse' smart constructor.
data UpdateApprovalRuleTemplateDescriptionResponse = UpdateApprovalRuleTemplateDescriptionResponse'
  { _uartdrsResponseStatus ::
      !Int,
    _uartdrsApprovalRuleTemplate ::
      !ApprovalRuleTemplate
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'UpdateApprovalRuleTemplateDescriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uartdrsResponseStatus' - -- | The response status code.
--
-- * 'uartdrsApprovalRuleTemplate' - The structure and content of the updated approval rule template.
updateApprovalRuleTemplateDescriptionResponse ::
  -- | 'uartdrsResponseStatus'
  Int ->
  -- | 'uartdrsApprovalRuleTemplate'
  ApprovalRuleTemplate ->
  UpdateApprovalRuleTemplateDescriptionResponse
updateApprovalRuleTemplateDescriptionResponse
  pResponseStatus_
  pApprovalRuleTemplate_ =
    UpdateApprovalRuleTemplateDescriptionResponse'
      { _uartdrsResponseStatus =
          pResponseStatus_,
        _uartdrsApprovalRuleTemplate =
          pApprovalRuleTemplate_
      }

-- | -- | The response status code.
uartdrsResponseStatus :: Lens' UpdateApprovalRuleTemplateDescriptionResponse Int
uartdrsResponseStatus = lens _uartdrsResponseStatus (\s a -> s {_uartdrsResponseStatus = a})

-- | The structure and content of the updated approval rule template.
uartdrsApprovalRuleTemplate :: Lens' UpdateApprovalRuleTemplateDescriptionResponse ApprovalRuleTemplate
uartdrsApprovalRuleTemplate = lens _uartdrsApprovalRuleTemplate (\s a -> s {_uartdrsApprovalRuleTemplate = a})

instance NFData UpdateApprovalRuleTemplateDescriptionResponse
