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
-- Module      : Amazonka.RBin.GetRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Recycle Bin retention rule.
module Amazonka.RBin.GetRule
  ( -- * Creating a Request
    GetRule (..),
    newGetRule,

    -- * Request Lenses
    getRule_identifier,

    -- * Destructuring the Response
    GetRuleResponse (..),
    newGetRuleResponse,

    -- * Response Lenses
    getRuleResponse_resourceType,
    getRuleResponse_status,
    getRuleResponse_resourceTags,
    getRuleResponse_description,
    getRuleResponse_retentionPeriod,
    getRuleResponse_identifier,
    getRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRule' smart constructor.
data GetRule = GetRule'
  { -- | The unique ID of the retention rule.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getRule_identifier' - The unique ID of the retention rule.
newGetRule ::
  -- | 'identifier'
  Prelude.Text ->
  GetRule
newGetRule pIdentifier_ =
  GetRule' {identifier = pIdentifier_}

-- | The unique ID of the retention rule.
getRule_identifier :: Lens.Lens' GetRule Prelude.Text
getRule_identifier = Lens.lens (\GetRule' {identifier} -> identifier) (\s@GetRule' {} a -> s {identifier = a} :: GetRule)

instance Core.AWSRequest GetRule where
  type AWSResponse GetRule = GetRuleResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleResponse'
            Prelude.<$> (x Core..?> "ResourceType")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ResourceTags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RetentionPeriod")
            Prelude.<*> (x Core..?> "Identifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRule where
  hashWithSalt _salt GetRule' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetRule where
  rnf GetRule' {..} = Prelude.rnf identifier

instance Core.ToHeaders GetRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetRule where
  toPath GetRule' {..} =
    Prelude.mconcat ["/rules/", Core.toBS identifier]

instance Core.ToQuery GetRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRuleResponse' smart constructor.
data GetRuleResponse = GetRuleResponse'
  { -- | The resource type retained by the retention rule.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The state of the retention rule. Only retention rules that are in the
    -- @available@ state retain resources.
    status :: Prelude.Maybe RuleStatus,
    -- | Information about the resource tags used to identify resources that are
    -- retained by the retention rule.
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | The retention rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the retention period for which the retention rule is
    -- to retain resources.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The unique ID of the retention rule.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'getRuleResponse_resourceType' - The resource type retained by the retention rule.
--
-- 'status', 'getRuleResponse_status' - The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
--
-- 'resourceTags', 'getRuleResponse_resourceTags' - Information about the resource tags used to identify resources that are
-- retained by the retention rule.
--
-- 'description', 'getRuleResponse_description' - The retention rule description.
--
-- 'retentionPeriod', 'getRuleResponse_retentionPeriod' - Information about the retention period for which the retention rule is
-- to retain resources.
--
-- 'identifier', 'getRuleResponse_identifier' - The unique ID of the retention rule.
--
-- 'httpStatus', 'getRuleResponse_httpStatus' - The response's http status code.
newGetRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRuleResponse
newGetRuleResponse pHttpStatus_ =
  GetRuleResponse'
    { resourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      description = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      identifier = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource type retained by the retention rule.
getRuleResponse_resourceType :: Lens.Lens' GetRuleResponse (Prelude.Maybe ResourceType)
getRuleResponse_resourceType = Lens.lens (\GetRuleResponse' {resourceType} -> resourceType) (\s@GetRuleResponse' {} a -> s {resourceType = a} :: GetRuleResponse)

-- | The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
getRuleResponse_status :: Lens.Lens' GetRuleResponse (Prelude.Maybe RuleStatus)
getRuleResponse_status = Lens.lens (\GetRuleResponse' {status} -> status) (\s@GetRuleResponse' {} a -> s {status = a} :: GetRuleResponse)

-- | Information about the resource tags used to identify resources that are
-- retained by the retention rule.
getRuleResponse_resourceTags :: Lens.Lens' GetRuleResponse (Prelude.Maybe [ResourceTag])
getRuleResponse_resourceTags = Lens.lens (\GetRuleResponse' {resourceTags} -> resourceTags) (\s@GetRuleResponse' {} a -> s {resourceTags = a} :: GetRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The retention rule description.
getRuleResponse_description :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.Text)
getRuleResponse_description = Lens.lens (\GetRuleResponse' {description} -> description) (\s@GetRuleResponse' {} a -> s {description = a} :: GetRuleResponse)

-- | Information about the retention period for which the retention rule is
-- to retain resources.
getRuleResponse_retentionPeriod :: Lens.Lens' GetRuleResponse (Prelude.Maybe RetentionPeriod)
getRuleResponse_retentionPeriod = Lens.lens (\GetRuleResponse' {retentionPeriod} -> retentionPeriod) (\s@GetRuleResponse' {} a -> s {retentionPeriod = a} :: GetRuleResponse)

-- | The unique ID of the retention rule.
getRuleResponse_identifier :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.Text)
getRuleResponse_identifier = Lens.lens (\GetRuleResponse' {identifier} -> identifier) (\s@GetRuleResponse' {} a -> s {identifier = a} :: GetRuleResponse)

-- | The response's http status code.
getRuleResponse_httpStatus :: Lens.Lens' GetRuleResponse Prelude.Int
getRuleResponse_httpStatus = Lens.lens (\GetRuleResponse' {httpStatus} -> httpStatus) (\s@GetRuleResponse' {} a -> s {httpStatus = a} :: GetRuleResponse)

instance Prelude.NFData GetRuleResponse where
  rnf GetRuleResponse' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf httpStatus
