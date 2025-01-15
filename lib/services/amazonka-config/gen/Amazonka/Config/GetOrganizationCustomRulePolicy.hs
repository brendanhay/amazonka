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
-- Module      : Amazonka.Config.GetOrganizationCustomRulePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the policy definition containing the logic for your organization
-- Config Custom Policy rule.
module Amazonka.Config.GetOrganizationCustomRulePolicy
  ( -- * Creating a Request
    GetOrganizationCustomRulePolicy (..),
    newGetOrganizationCustomRulePolicy,

    -- * Request Lenses
    getOrganizationCustomRulePolicy_organizationConfigRuleName,

    -- * Destructuring the Response
    GetOrganizationCustomRulePolicyResponse (..),
    newGetOrganizationCustomRulePolicyResponse,

    -- * Response Lenses
    getOrganizationCustomRulePolicyResponse_policyText,
    getOrganizationCustomRulePolicyResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOrganizationCustomRulePolicy' smart constructor.
data GetOrganizationCustomRulePolicy = GetOrganizationCustomRulePolicy'
  { -- | The name of your organization Config Custom Policy rule.
    organizationConfigRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOrganizationCustomRulePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationConfigRuleName', 'getOrganizationCustomRulePolicy_organizationConfigRuleName' - The name of your organization Config Custom Policy rule.
newGetOrganizationCustomRulePolicy ::
  -- | 'organizationConfigRuleName'
  Prelude.Text ->
  GetOrganizationCustomRulePolicy
newGetOrganizationCustomRulePolicy
  pOrganizationConfigRuleName_ =
    GetOrganizationCustomRulePolicy'
      { organizationConfigRuleName =
          pOrganizationConfigRuleName_
      }

-- | The name of your organization Config Custom Policy rule.
getOrganizationCustomRulePolicy_organizationConfigRuleName :: Lens.Lens' GetOrganizationCustomRulePolicy Prelude.Text
getOrganizationCustomRulePolicy_organizationConfigRuleName = Lens.lens (\GetOrganizationCustomRulePolicy' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@GetOrganizationCustomRulePolicy' {} a -> s {organizationConfigRuleName = a} :: GetOrganizationCustomRulePolicy)

instance
  Core.AWSRequest
    GetOrganizationCustomRulePolicy
  where
  type
    AWSResponse GetOrganizationCustomRulePolicy =
      GetOrganizationCustomRulePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOrganizationCustomRulePolicyResponse'
            Prelude.<$> (x Data..?> "PolicyText")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetOrganizationCustomRulePolicy
  where
  hashWithSalt
    _salt
    GetOrganizationCustomRulePolicy' {..} =
      _salt
        `Prelude.hashWithSalt` organizationConfigRuleName

instance
  Prelude.NFData
    GetOrganizationCustomRulePolicy
  where
  rnf GetOrganizationCustomRulePolicy' {..} =
    Prelude.rnf organizationConfigRuleName

instance
  Data.ToHeaders
    GetOrganizationCustomRulePolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetOrganizationCustomRulePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetOrganizationCustomRulePolicy where
  toJSON GetOrganizationCustomRulePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "OrganizationConfigRuleName"
                  Data..= organizationConfigRuleName
              )
          ]
      )

instance Data.ToPath GetOrganizationCustomRulePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetOrganizationCustomRulePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOrganizationCustomRulePolicyResponse' smart constructor.
data GetOrganizationCustomRulePolicyResponse = GetOrganizationCustomRulePolicyResponse'
  { -- | The policy definition containing the logic for your organization Config
    -- Custom Policy rule.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOrganizationCustomRulePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyText', 'getOrganizationCustomRulePolicyResponse_policyText' - The policy definition containing the logic for your organization Config
-- Custom Policy rule.
--
-- 'httpStatus', 'getOrganizationCustomRulePolicyResponse_httpStatus' - The response's http status code.
newGetOrganizationCustomRulePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOrganizationCustomRulePolicyResponse
newGetOrganizationCustomRulePolicyResponse
  pHttpStatus_ =
    GetOrganizationCustomRulePolicyResponse'
      { policyText =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The policy definition containing the logic for your organization Config
-- Custom Policy rule.
getOrganizationCustomRulePolicyResponse_policyText :: Lens.Lens' GetOrganizationCustomRulePolicyResponse (Prelude.Maybe Prelude.Text)
getOrganizationCustomRulePolicyResponse_policyText = Lens.lens (\GetOrganizationCustomRulePolicyResponse' {policyText} -> policyText) (\s@GetOrganizationCustomRulePolicyResponse' {} a -> s {policyText = a} :: GetOrganizationCustomRulePolicyResponse)

-- | The response's http status code.
getOrganizationCustomRulePolicyResponse_httpStatus :: Lens.Lens' GetOrganizationCustomRulePolicyResponse Prelude.Int
getOrganizationCustomRulePolicyResponse_httpStatus = Lens.lens (\GetOrganizationCustomRulePolicyResponse' {httpStatus} -> httpStatus) (\s@GetOrganizationCustomRulePolicyResponse' {} a -> s {httpStatus = a} :: GetOrganizationCustomRulePolicyResponse)

instance
  Prelude.NFData
    GetOrganizationCustomRulePolicyResponse
  where
  rnf GetOrganizationCustomRulePolicyResponse' {..} =
    Prelude.rnf policyText `Prelude.seq`
      Prelude.rnf httpStatus
