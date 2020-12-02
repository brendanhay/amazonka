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
-- Module      : Network.AWS.WorkMail.DeleteAccessControlRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an access control rule for the specified WorkMail organization.
module Network.AWS.WorkMail.DeleteAccessControlRule
  ( -- * Creating a Request
    deleteAccessControlRule,
    DeleteAccessControlRule,

    -- * Request Lenses
    dacrOrganizationId,
    dacrName,

    -- * Destructuring the Response
    deleteAccessControlRuleResponse,
    DeleteAccessControlRuleResponse,

    -- * Response Lenses
    dacrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'deleteAccessControlRule' smart constructor.
data DeleteAccessControlRule = DeleteAccessControlRule'
  { _dacrOrganizationId ::
      !Text,
    _dacrName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAccessControlRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacrOrganizationId' - The identifier for the organization.
--
-- * 'dacrName' - The name of the access control rule.
deleteAccessControlRule ::
  -- | 'dacrOrganizationId'
  Text ->
  -- | 'dacrName'
  Text ->
  DeleteAccessControlRule
deleteAccessControlRule pOrganizationId_ pName_ =
  DeleteAccessControlRule'
    { _dacrOrganizationId = pOrganizationId_,
      _dacrName = pName_
    }

-- | The identifier for the organization.
dacrOrganizationId :: Lens' DeleteAccessControlRule Text
dacrOrganizationId = lens _dacrOrganizationId (\s a -> s {_dacrOrganizationId = a})

-- | The name of the access control rule.
dacrName :: Lens' DeleteAccessControlRule Text
dacrName = lens _dacrName (\s a -> s {_dacrName = a})

instance AWSRequest DeleteAccessControlRule where
  type Rs DeleteAccessControlRule = DeleteAccessControlRuleResponse
  request = postJSON workMail
  response =
    receiveEmpty
      ( \s h x ->
          DeleteAccessControlRuleResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteAccessControlRule

instance NFData DeleteAccessControlRule

instance ToHeaders DeleteAccessControlRule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.DeleteAccessControlRule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAccessControlRule where
  toJSON DeleteAccessControlRule' {..} =
    object
      ( catMaybes
          [ Just ("OrganizationId" .= _dacrOrganizationId),
            Just ("Name" .= _dacrName)
          ]
      )

instance ToPath DeleteAccessControlRule where
  toPath = const "/"

instance ToQuery DeleteAccessControlRule where
  toQuery = const mempty

-- | /See:/ 'deleteAccessControlRuleResponse' smart constructor.
newtype DeleteAccessControlRuleResponse = DeleteAccessControlRuleResponse'
  { _dacrrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAccessControlRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dacrrsResponseStatus' - -- | The response status code.
deleteAccessControlRuleResponse ::
  -- | 'dacrrsResponseStatus'
  Int ->
  DeleteAccessControlRuleResponse
deleteAccessControlRuleResponse pResponseStatus_ =
  DeleteAccessControlRuleResponse'
    { _dacrrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dacrrsResponseStatus :: Lens' DeleteAccessControlRuleResponse Int
dacrrsResponseStatus = lens _dacrrsResponseStatus (\s a -> s {_dacrrsResponseStatus = a})

instance NFData DeleteAccessControlRuleResponse
