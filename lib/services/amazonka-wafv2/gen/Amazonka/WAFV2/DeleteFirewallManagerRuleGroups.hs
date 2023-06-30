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
-- Module      : Amazonka.WAFV2.DeleteFirewallManagerRuleGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all rule groups that are managed by Firewall Manager for the
-- specified web ACL.
--
-- You can only use this if @ManagedByFirewallManager@ is false in the
-- specified WebACL.
module Amazonka.WAFV2.DeleteFirewallManagerRuleGroups
  ( -- * Creating a Request
    DeleteFirewallManagerRuleGroups (..),
    newDeleteFirewallManagerRuleGroups,

    -- * Request Lenses
    deleteFirewallManagerRuleGroups_webACLArn,
    deleteFirewallManagerRuleGroups_webACLLockToken,

    -- * Destructuring the Response
    DeleteFirewallManagerRuleGroupsResponse (..),
    newDeleteFirewallManagerRuleGroupsResponse,

    -- * Response Lenses
    deleteFirewallManagerRuleGroupsResponse_nextWebACLLockToken,
    deleteFirewallManagerRuleGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDeleteFirewallManagerRuleGroups' smart constructor.
data DeleteFirewallManagerRuleGroups = DeleteFirewallManagerRuleGroups'
  { -- | The Amazon Resource Name (ARN) of the web ACL.
    webACLArn :: Prelude.Text,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    webACLLockToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallManagerRuleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACLArn', 'deleteFirewallManagerRuleGroups_webACLArn' - The Amazon Resource Name (ARN) of the web ACL.
--
-- 'webACLLockToken', 'deleteFirewallManagerRuleGroups_webACLLockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
newDeleteFirewallManagerRuleGroups ::
  -- | 'webACLArn'
  Prelude.Text ->
  -- | 'webACLLockToken'
  Prelude.Text ->
  DeleteFirewallManagerRuleGroups
newDeleteFirewallManagerRuleGroups
  pWebACLArn_
  pWebACLLockToken_ =
    DeleteFirewallManagerRuleGroups'
      { webACLArn =
          pWebACLArn_,
        webACLLockToken = pWebACLLockToken_
      }

-- | The Amazon Resource Name (ARN) of the web ACL.
deleteFirewallManagerRuleGroups_webACLArn :: Lens.Lens' DeleteFirewallManagerRuleGroups Prelude.Text
deleteFirewallManagerRuleGroups_webACLArn = Lens.lens (\DeleteFirewallManagerRuleGroups' {webACLArn} -> webACLArn) (\s@DeleteFirewallManagerRuleGroups' {} a -> s {webACLArn = a} :: DeleteFirewallManagerRuleGroups)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
deleteFirewallManagerRuleGroups_webACLLockToken :: Lens.Lens' DeleteFirewallManagerRuleGroups Prelude.Text
deleteFirewallManagerRuleGroups_webACLLockToken = Lens.lens (\DeleteFirewallManagerRuleGroups' {webACLLockToken} -> webACLLockToken) (\s@DeleteFirewallManagerRuleGroups' {} a -> s {webACLLockToken = a} :: DeleteFirewallManagerRuleGroups)

instance
  Core.AWSRequest
    DeleteFirewallManagerRuleGroups
  where
  type
    AWSResponse DeleteFirewallManagerRuleGroups =
      DeleteFirewallManagerRuleGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFirewallManagerRuleGroupsResponse'
            Prelude.<$> (x Data..?> "NextWebACLLockToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteFirewallManagerRuleGroups
  where
  hashWithSalt
    _salt
    DeleteFirewallManagerRuleGroups' {..} =
      _salt
        `Prelude.hashWithSalt` webACLArn
        `Prelude.hashWithSalt` webACLLockToken

instance
  Prelude.NFData
    DeleteFirewallManagerRuleGroups
  where
  rnf DeleteFirewallManagerRuleGroups' {..} =
    Prelude.rnf webACLArn
      `Prelude.seq` Prelude.rnf webACLLockToken

instance
  Data.ToHeaders
    DeleteFirewallManagerRuleGroups
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.DeleteFirewallManagerRuleGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFirewallManagerRuleGroups where
  toJSON DeleteFirewallManagerRuleGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WebACLArn" Data..= webACLArn),
            Prelude.Just
              ("WebACLLockToken" Data..= webACLLockToken)
          ]
      )

instance Data.ToPath DeleteFirewallManagerRuleGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFirewallManagerRuleGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFirewallManagerRuleGroupsResponse' smart constructor.
data DeleteFirewallManagerRuleGroupsResponse = DeleteFirewallManagerRuleGroupsResponse'
  { -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    nextWebACLLockToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFirewallManagerRuleGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextWebACLLockToken', 'deleteFirewallManagerRuleGroupsResponse_nextWebACLLockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'httpStatus', 'deleteFirewallManagerRuleGroupsResponse_httpStatus' - The response's http status code.
newDeleteFirewallManagerRuleGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFirewallManagerRuleGroupsResponse
newDeleteFirewallManagerRuleGroupsResponse
  pHttpStatus_ =
    DeleteFirewallManagerRuleGroupsResponse'
      { nextWebACLLockToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
deleteFirewallManagerRuleGroupsResponse_nextWebACLLockToken :: Lens.Lens' DeleteFirewallManagerRuleGroupsResponse (Prelude.Maybe Prelude.Text)
deleteFirewallManagerRuleGroupsResponse_nextWebACLLockToken = Lens.lens (\DeleteFirewallManagerRuleGroupsResponse' {nextWebACLLockToken} -> nextWebACLLockToken) (\s@DeleteFirewallManagerRuleGroupsResponse' {} a -> s {nextWebACLLockToken = a} :: DeleteFirewallManagerRuleGroupsResponse)

-- | The response's http status code.
deleteFirewallManagerRuleGroupsResponse_httpStatus :: Lens.Lens' DeleteFirewallManagerRuleGroupsResponse Prelude.Int
deleteFirewallManagerRuleGroupsResponse_httpStatus = Lens.lens (\DeleteFirewallManagerRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@DeleteFirewallManagerRuleGroupsResponse' {} a -> s {httpStatus = a} :: DeleteFirewallManagerRuleGroupsResponse)

instance
  Prelude.NFData
    DeleteFirewallManagerRuleGroupsResponse
  where
  rnf DeleteFirewallManagerRuleGroupsResponse' {..} =
    Prelude.rnf nextWebACLLockToken
      `Prelude.seq` Prelude.rnf httpStatus
