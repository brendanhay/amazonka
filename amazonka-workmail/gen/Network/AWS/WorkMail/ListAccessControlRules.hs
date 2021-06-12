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
-- Module      : Network.AWS.WorkMail.ListAccessControlRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the access control rules for the specified organization.
module Network.AWS.WorkMail.ListAccessControlRules
  ( -- * Creating a Request
    ListAccessControlRules (..),
    newListAccessControlRules,

    -- * Request Lenses
    listAccessControlRules_organizationId,

    -- * Destructuring the Response
    ListAccessControlRulesResponse (..),
    newListAccessControlRulesResponse,

    -- * Response Lenses
    listAccessControlRulesResponse_rules,
    listAccessControlRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListAccessControlRules' smart constructor.
data ListAccessControlRules = ListAccessControlRules'
  { -- | The identifier for the organization.
    organizationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAccessControlRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'listAccessControlRules_organizationId' - The identifier for the organization.
newListAccessControlRules ::
  -- | 'organizationId'
  Core.Text ->
  ListAccessControlRules
newListAccessControlRules pOrganizationId_ =
  ListAccessControlRules'
    { organizationId =
        pOrganizationId_
    }

-- | The identifier for the organization.
listAccessControlRules_organizationId :: Lens.Lens' ListAccessControlRules Core.Text
listAccessControlRules_organizationId = Lens.lens (\ListAccessControlRules' {organizationId} -> organizationId) (\s@ListAccessControlRules' {} a -> s {organizationId = a} :: ListAccessControlRules)

instance Core.AWSRequest ListAccessControlRules where
  type
    AWSResponse ListAccessControlRules =
      ListAccessControlRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccessControlRulesResponse'
            Core.<$> (x Core..?> "Rules" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAccessControlRules

instance Core.NFData ListAccessControlRules

instance Core.ToHeaders ListAccessControlRules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListAccessControlRules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAccessControlRules where
  toJSON ListAccessControlRules' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath ListAccessControlRules where
  toPath = Core.const "/"

instance Core.ToQuery ListAccessControlRules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAccessControlRulesResponse' smart constructor.
data ListAccessControlRulesResponse = ListAccessControlRulesResponse'
  { -- | The access control rules.
    rules :: Core.Maybe [AccessControlRule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAccessControlRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'listAccessControlRulesResponse_rules' - The access control rules.
--
-- 'httpStatus', 'listAccessControlRulesResponse_httpStatus' - The response's http status code.
newListAccessControlRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAccessControlRulesResponse
newListAccessControlRulesResponse pHttpStatus_ =
  ListAccessControlRulesResponse'
    { rules =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The access control rules.
listAccessControlRulesResponse_rules :: Lens.Lens' ListAccessControlRulesResponse (Core.Maybe [AccessControlRule])
listAccessControlRulesResponse_rules = Lens.lens (\ListAccessControlRulesResponse' {rules} -> rules) (\s@ListAccessControlRulesResponse' {} a -> s {rules = a} :: ListAccessControlRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAccessControlRulesResponse_httpStatus :: Lens.Lens' ListAccessControlRulesResponse Core.Int
listAccessControlRulesResponse_httpStatus = Lens.lens (\ListAccessControlRulesResponse' {httpStatus} -> httpStatus) (\s@ListAccessControlRulesResponse' {} a -> s {httpStatus = a} :: ListAccessControlRulesResponse)

instance Core.NFData ListAccessControlRulesResponse
