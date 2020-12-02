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
-- Module      : Network.AWS.CodeCommit.GetApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified approval rule template.
module Network.AWS.CodeCommit.GetApprovalRuleTemplate
  ( -- * Creating a Request
    getApprovalRuleTemplate,
    GetApprovalRuleTemplate,

    -- * Request Lenses
    gartApprovalRuleTemplateName,

    -- * Destructuring the Response
    getApprovalRuleTemplateResponse,
    GetApprovalRuleTemplateResponse,

    -- * Response Lenses
    gartrsResponseStatus,
    gartrsApprovalRuleTemplate,
  )
where

import Network.AWS.CodeCommit.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getApprovalRuleTemplate' smart constructor.
newtype GetApprovalRuleTemplate = GetApprovalRuleTemplate'
  { _gartApprovalRuleTemplateName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gartApprovalRuleTemplateName' - The name of the approval rule template for which you want to get information.
getApprovalRuleTemplate ::
  -- | 'gartApprovalRuleTemplateName'
  Text ->
  GetApprovalRuleTemplate
getApprovalRuleTemplate pApprovalRuleTemplateName_ =
  GetApprovalRuleTemplate'
    { _gartApprovalRuleTemplateName =
        pApprovalRuleTemplateName_
    }

-- | The name of the approval rule template for which you want to get information.
gartApprovalRuleTemplateName :: Lens' GetApprovalRuleTemplate Text
gartApprovalRuleTemplateName = lens _gartApprovalRuleTemplateName (\s a -> s {_gartApprovalRuleTemplateName = a})

instance AWSRequest GetApprovalRuleTemplate where
  type Rs GetApprovalRuleTemplate = GetApprovalRuleTemplateResponse
  request = postJSON codeCommit
  response =
    receiveJSON
      ( \s h x ->
          GetApprovalRuleTemplateResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "approvalRuleTemplate")
      )

instance Hashable GetApprovalRuleTemplate

instance NFData GetApprovalRuleTemplate

instance ToHeaders GetApprovalRuleTemplate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeCommit_20150413.GetApprovalRuleTemplate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetApprovalRuleTemplate where
  toJSON GetApprovalRuleTemplate' {..} =
    object
      ( catMaybes
          [ Just
              ("approvalRuleTemplateName" .= _gartApprovalRuleTemplateName)
          ]
      )

instance ToPath GetApprovalRuleTemplate where
  toPath = const "/"

instance ToQuery GetApprovalRuleTemplate where
  toQuery = const mempty

-- | /See:/ 'getApprovalRuleTemplateResponse' smart constructor.
data GetApprovalRuleTemplateResponse = GetApprovalRuleTemplateResponse'
  { _gartrsResponseStatus ::
      !Int,
    _gartrsApprovalRuleTemplate ::
      !ApprovalRuleTemplate
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetApprovalRuleTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gartrsResponseStatus' - -- | The response status code.
--
-- * 'gartrsApprovalRuleTemplate' - The content and structure of the approval rule template.
getApprovalRuleTemplateResponse ::
  -- | 'gartrsResponseStatus'
  Int ->
  -- | 'gartrsApprovalRuleTemplate'
  ApprovalRuleTemplate ->
  GetApprovalRuleTemplateResponse
getApprovalRuleTemplateResponse
  pResponseStatus_
  pApprovalRuleTemplate_ =
    GetApprovalRuleTemplateResponse'
      { _gartrsResponseStatus =
          pResponseStatus_,
        _gartrsApprovalRuleTemplate = pApprovalRuleTemplate_
      }

-- | -- | The response status code.
gartrsResponseStatus :: Lens' GetApprovalRuleTemplateResponse Int
gartrsResponseStatus = lens _gartrsResponseStatus (\s a -> s {_gartrsResponseStatus = a})

-- | The content and structure of the approval rule template.
gartrsApprovalRuleTemplate :: Lens' GetApprovalRuleTemplateResponse ApprovalRuleTemplate
gartrsApprovalRuleTemplate = lens _gartrsApprovalRuleTemplate (\s a -> s {_gartrsApprovalRuleTemplate = a})

instance NFData GetApprovalRuleTemplateResponse
