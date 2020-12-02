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
-- Module      : Network.AWS.Config.DeleteOrganizationConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization config rule and all of its evaluation results from all member accounts in that organization.
--
--
-- Only a master account and a delegated administrator account can delete an organization config rule. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added.
--
-- AWS Config sets the state of a rule to DELETE_IN_PROGRESS until the deletion is complete. You cannot update a rule while it is in this state.
module Network.AWS.Config.DeleteOrganizationConfigRule
  ( -- * Creating a Request
    deleteOrganizationConfigRule,
    DeleteOrganizationConfigRule,

    -- * Request Lenses
    docrOrganizationConfigRuleName,

    -- * Destructuring the Response
    deleteOrganizationConfigRuleResponse,
    DeleteOrganizationConfigRuleResponse,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteOrganizationConfigRule' smart constructor.
newtype DeleteOrganizationConfigRule = DeleteOrganizationConfigRule'
  { _docrOrganizationConfigRuleName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteOrganizationConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docrOrganizationConfigRuleName' - The name of organization config rule that you want to delete.
deleteOrganizationConfigRule ::
  -- | 'docrOrganizationConfigRuleName'
  Text ->
  DeleteOrganizationConfigRule
deleteOrganizationConfigRule pOrganizationConfigRuleName_ =
  DeleteOrganizationConfigRule'
    { _docrOrganizationConfigRuleName =
        pOrganizationConfigRuleName_
    }

-- | The name of organization config rule that you want to delete.
docrOrganizationConfigRuleName :: Lens' DeleteOrganizationConfigRule Text
docrOrganizationConfigRuleName = lens _docrOrganizationConfigRuleName (\s a -> s {_docrOrganizationConfigRuleName = a})

instance AWSRequest DeleteOrganizationConfigRule where
  type
    Rs DeleteOrganizationConfigRule =
      DeleteOrganizationConfigRuleResponse
  request = postJSON config
  response = receiveNull DeleteOrganizationConfigRuleResponse'

instance Hashable DeleteOrganizationConfigRule

instance NFData DeleteOrganizationConfigRule

instance ToHeaders DeleteOrganizationConfigRule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.DeleteOrganizationConfigRule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteOrganizationConfigRule where
  toJSON DeleteOrganizationConfigRule' {..} =
    object
      ( catMaybes
          [ Just
              ("OrganizationConfigRuleName" .= _docrOrganizationConfigRuleName)
          ]
      )

instance ToPath DeleteOrganizationConfigRule where
  toPath = const "/"

instance ToQuery DeleteOrganizationConfigRule where
  toQuery = const mempty

-- | /See:/ 'deleteOrganizationConfigRuleResponse' smart constructor.
data DeleteOrganizationConfigRuleResponse = DeleteOrganizationConfigRuleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteOrganizationConfigRuleResponse' with the minimum fields required to make a request.
deleteOrganizationConfigRuleResponse ::
  DeleteOrganizationConfigRuleResponse
deleteOrganizationConfigRuleResponse =
  DeleteOrganizationConfigRuleResponse'

instance NFData DeleteOrganizationConfigRuleResponse
