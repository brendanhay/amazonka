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
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a specified approval rule template.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
  ( -- * Creating a Request
    updateApprovalRuleTemplateName,
    UpdateApprovalRuleTemplateName,

    -- * Request Lenses
    uartnOldApprovalRuleTemplateName,
    uartnNewApprovalRuleTemplateName,

    -- * Destructuring the Response
    updateApprovalRuleTemplateNameResponse,
    UpdateApprovalRuleTemplateNameResponse,

    -- * Response Lenses
    uartnrsResponseStatus,
    uartnrsApprovalRuleTemplate,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateApprovalRuleTemplateName' smart constructor.
data UpdateApprovalRuleTemplateName = UpdateApprovalRuleTemplateName'
  { _uartnOldApprovalRuleTemplateName ::
      !Text,
    _uartnNewApprovalRuleTemplateName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateApprovalRuleTemplateName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uartnOldApprovalRuleTemplateName' - The current name of the approval rule template.
--
-- * 'uartnNewApprovalRuleTemplateName' - The new name you want to apply to the approval rule template.
updateApprovalRuleTemplateName ::
  -- | 'uartnOldApprovalRuleTemplateName'
  Text ->
  -- | 'uartnNewApprovalRuleTemplateName'
  Text ->
  UpdateApprovalRuleTemplateName
updateApprovalRuleTemplateName
  pOldApprovalRuleTemplateName_
  pNewApprovalRuleTemplateName_ =
    UpdateApprovalRuleTemplateName'
      { _uartnOldApprovalRuleTemplateName =
          pOldApprovalRuleTemplateName_,
        _uartnNewApprovalRuleTemplateName =
          pNewApprovalRuleTemplateName_
      }

-- | The current name of the approval rule template.
uartnOldApprovalRuleTemplateName :: Lens' UpdateApprovalRuleTemplateName Text
uartnOldApprovalRuleTemplateName = lens _uartnOldApprovalRuleTemplateName (\s a -> s {_uartnOldApprovalRuleTemplateName = a})

-- | The new name you want to apply to the approval rule template.
uartnNewApprovalRuleTemplateName :: Lens' UpdateApprovalRuleTemplateName Text
uartnNewApprovalRuleTemplateName = lens _uartnNewApprovalRuleTemplateName (\s a -> s {_uartnNewApprovalRuleTemplateName = a})

instance AWSRequest UpdateApprovalRuleTemplateName where
  type
    Rs UpdateApprovalRuleTemplateName =
      UpdateApprovalRuleTemplateNameResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateNameResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "approvalRuleTemplate")
      )

instance Hashable UpdateApprovalRuleTemplateName

instance NFData UpdateApprovalRuleTemplateName

instance ToHeaders UpdateApprovalRuleTemplateName where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "CodeCommit_20150413.UpdateApprovalRuleTemplateName" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateApprovalRuleTemplateName where
  toJSON UpdateApprovalRuleTemplateName' {..} =
    object
      ( catMaybes
          [ Just
              ( "oldApprovalRuleTemplateName"
                  .= _uartnOldApprovalRuleTemplateName
              ),
            Just
              ( "newApprovalRuleTemplateName"
                  .= _uartnNewApprovalRuleTemplateName
              )
          ]
      )

instance ToPath UpdateApprovalRuleTemplateName where
  toPath = const "/"

instance ToQuery UpdateApprovalRuleTemplateName where
  toQuery = const mempty

-- | /See:/ 'updateApprovalRuleTemplateNameResponse' smart constructor.
data UpdateApprovalRuleTemplateNameResponse = UpdateApprovalRuleTemplateNameResponse'
  { _uartnrsResponseStatus ::
      !Int,
    _uartnrsApprovalRuleTemplate ::
      !ApprovalRuleTemplate
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateApprovalRuleTemplateNameResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uartnrsResponseStatus' - -- | The response status code.
--
-- * 'uartnrsApprovalRuleTemplate' - The structure and content of the updated approval rule template.
updateApprovalRuleTemplateNameResponse ::
  -- | 'uartnrsResponseStatus'
  Int ->
  -- | 'uartnrsApprovalRuleTemplate'
  ApprovalRuleTemplate ->
  UpdateApprovalRuleTemplateNameResponse
updateApprovalRuleTemplateNameResponse
  pResponseStatus_
  pApprovalRuleTemplate_ =
    UpdateApprovalRuleTemplateNameResponse'
      { _uartnrsResponseStatus =
          pResponseStatus_,
        _uartnrsApprovalRuleTemplate = pApprovalRuleTemplate_
      }

-- | -- | The response status code.
uartnrsResponseStatus :: Lens' UpdateApprovalRuleTemplateNameResponse Int
uartnrsResponseStatus = lens _uartnrsResponseStatus (\s a -> s {_uartnrsResponseStatus = a})

-- | The structure and content of the updated approval rule template.
uartnrsApprovalRuleTemplate :: Lens' UpdateApprovalRuleTemplateNameResponse ApprovalRuleTemplate
uartnrsApprovalRuleTemplate = lens _uartnrsApprovalRuleTemplate (\s a -> s {_uartnrsApprovalRuleTemplate = a})

instance NFData UpdateApprovalRuleTemplateNameResponse
