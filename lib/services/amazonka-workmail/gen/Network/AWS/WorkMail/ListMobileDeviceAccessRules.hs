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
-- Module      : Network.AWS.WorkMail.ListMobileDeviceAccessRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mobile device access rules for the specified Amazon WorkMail
-- organization.
module Network.AWS.WorkMail.ListMobileDeviceAccessRules
  ( -- * Creating a Request
    ListMobileDeviceAccessRules (..),
    newListMobileDeviceAccessRules,

    -- * Request Lenses
    listMobileDeviceAccessRules_organizationId,

    -- * Destructuring the Response
    ListMobileDeviceAccessRulesResponse (..),
    newListMobileDeviceAccessRulesResponse,

    -- * Response Lenses
    listMobileDeviceAccessRulesResponse_rules,
    listMobileDeviceAccessRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newListMobileDeviceAccessRules' smart constructor.
data ListMobileDeviceAccessRules = ListMobileDeviceAccessRules'
  { -- | The Amazon WorkMail organization for which to list the rules.
    organizationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMobileDeviceAccessRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'listMobileDeviceAccessRules_organizationId' - The Amazon WorkMail organization for which to list the rules.
newListMobileDeviceAccessRules ::
  -- | 'organizationId'
  Prelude.Text ->
  ListMobileDeviceAccessRules
newListMobileDeviceAccessRules pOrganizationId_ =
  ListMobileDeviceAccessRules'
    { organizationId =
        pOrganizationId_
    }

-- | The Amazon WorkMail organization for which to list the rules.
listMobileDeviceAccessRules_organizationId :: Lens.Lens' ListMobileDeviceAccessRules Prelude.Text
listMobileDeviceAccessRules_organizationId = Lens.lens (\ListMobileDeviceAccessRules' {organizationId} -> organizationId) (\s@ListMobileDeviceAccessRules' {} a -> s {organizationId = a} :: ListMobileDeviceAccessRules)

instance Core.AWSRequest ListMobileDeviceAccessRules where
  type
    AWSResponse ListMobileDeviceAccessRules =
      ListMobileDeviceAccessRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMobileDeviceAccessRulesResponse'
            Prelude.<$> (x Core..?> "Rules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMobileDeviceAccessRules

instance Prelude.NFData ListMobileDeviceAccessRules

instance Core.ToHeaders ListMobileDeviceAccessRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.ListMobileDeviceAccessRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMobileDeviceAccessRules where
  toJSON ListMobileDeviceAccessRules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Core..= organizationId)
          ]
      )

instance Core.ToPath ListMobileDeviceAccessRules where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMobileDeviceAccessRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMobileDeviceAccessRulesResponse' smart constructor.
data ListMobileDeviceAccessRulesResponse = ListMobileDeviceAccessRulesResponse'
  { -- | The list of mobile device access rules that exist under the specified
    -- Amazon WorkMail organization.
    rules :: Prelude.Maybe [MobileDeviceAccessRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMobileDeviceAccessRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'listMobileDeviceAccessRulesResponse_rules' - The list of mobile device access rules that exist under the specified
-- Amazon WorkMail organization.
--
-- 'httpStatus', 'listMobileDeviceAccessRulesResponse_httpStatus' - The response's http status code.
newListMobileDeviceAccessRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMobileDeviceAccessRulesResponse
newListMobileDeviceAccessRulesResponse pHttpStatus_ =
  ListMobileDeviceAccessRulesResponse'
    { rules =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of mobile device access rules that exist under the specified
-- Amazon WorkMail organization.
listMobileDeviceAccessRulesResponse_rules :: Lens.Lens' ListMobileDeviceAccessRulesResponse (Prelude.Maybe [MobileDeviceAccessRule])
listMobileDeviceAccessRulesResponse_rules = Lens.lens (\ListMobileDeviceAccessRulesResponse' {rules} -> rules) (\s@ListMobileDeviceAccessRulesResponse' {} a -> s {rules = a} :: ListMobileDeviceAccessRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMobileDeviceAccessRulesResponse_httpStatus :: Lens.Lens' ListMobileDeviceAccessRulesResponse Prelude.Int
listMobileDeviceAccessRulesResponse_httpStatus = Lens.lens (\ListMobileDeviceAccessRulesResponse' {httpStatus} -> httpStatus) (\s@ListMobileDeviceAccessRulesResponse' {} a -> s {httpStatus = a} :: ListMobileDeviceAccessRulesResponse)

instance
  Prelude.NFData
    ListMobileDeviceAccessRulesResponse
