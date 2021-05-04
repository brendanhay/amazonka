{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteOrganizationConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified organization config rule and all of its evaluation
-- results from all member accounts in that organization.
--
-- Only a master account and a delegated administrator account can delete
-- an organization config rule. When calling this API with a delegated
-- administrator, you must ensure AWS Organizations
-- @ListDelegatedAdministrator@ permissions are added.
--
-- AWS Config sets the state of a rule to DELETE_IN_PROGRESS until the
-- deletion is complete. You cannot update a rule while it is in this
-- state.
module Network.AWS.Config.DeleteOrganizationConfigRule
  ( -- * Creating a Request
    DeleteOrganizationConfigRule (..),
    newDeleteOrganizationConfigRule,

    -- * Request Lenses
    deleteOrganizationConfigRule_organizationConfigRuleName,

    -- * Destructuring the Response
    DeleteOrganizationConfigRuleResponse (..),
    newDeleteOrganizationConfigRuleResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteOrganizationConfigRule' smart constructor.
data DeleteOrganizationConfigRule = DeleteOrganizationConfigRule'
  { -- | The name of organization config rule that you want to delete.
    organizationConfigRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganizationConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationConfigRuleName', 'deleteOrganizationConfigRule_organizationConfigRuleName' - The name of organization config rule that you want to delete.
newDeleteOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Prelude.Text ->
  DeleteOrganizationConfigRule
newDeleteOrganizationConfigRule
  pOrganizationConfigRuleName_ =
    DeleteOrganizationConfigRule'
      { organizationConfigRuleName =
          pOrganizationConfigRuleName_
      }

-- | The name of organization config rule that you want to delete.
deleteOrganizationConfigRule_organizationConfigRuleName :: Lens.Lens' DeleteOrganizationConfigRule Prelude.Text
deleteOrganizationConfigRule_organizationConfigRuleName = Lens.lens (\DeleteOrganizationConfigRule' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@DeleteOrganizationConfigRule' {} a -> s {organizationConfigRuleName = a} :: DeleteOrganizationConfigRule)

instance
  Prelude.AWSRequest
    DeleteOrganizationConfigRule
  where
  type
    Rs DeleteOrganizationConfigRule =
      DeleteOrganizationConfigRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteOrganizationConfigRuleResponse'

instance
  Prelude.Hashable
    DeleteOrganizationConfigRule

instance Prelude.NFData DeleteOrganizationConfigRule

instance
  Prelude.ToHeaders
    DeleteOrganizationConfigRule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeleteOrganizationConfigRule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteOrganizationConfigRule where
  toJSON DeleteOrganizationConfigRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OrganizationConfigRuleName"
                  Prelude..= organizationConfigRuleName
              )
          ]
      )

instance Prelude.ToPath DeleteOrganizationConfigRule where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteOrganizationConfigRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOrganizationConfigRuleResponse' smart constructor.
data DeleteOrganizationConfigRuleResponse = DeleteOrganizationConfigRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOrganizationConfigRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOrganizationConfigRuleResponse ::
  DeleteOrganizationConfigRuleResponse
newDeleteOrganizationConfigRuleResponse =
  DeleteOrganizationConfigRuleResponse'

instance
  Prelude.NFData
    DeleteOrganizationConfigRuleResponse
