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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getRuleResponse_lockState,
    getRuleResponse_lockConfiguration,
    getRuleResponse_status,
    getRuleResponse_resourceTags,
    getRuleResponse_description,
    getRuleResponse_lockEndTime,
    getRuleResponse_retentionPeriod,
    getRuleResponse_identifier,
    getRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleResponse'
            Prelude.<$> (x Data..?> "ResourceType")
            Prelude.<*> (x Data..?> "LockState")
            Prelude.<*> (x Data..?> "LockConfiguration")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "ResourceTags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LockEndTime")
            Prelude.<*> (x Data..?> "RetentionPeriod")
            Prelude.<*> (x Data..?> "Identifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRule where
  hashWithSalt _salt GetRule' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetRule where
  rnf GetRule' {..} = Prelude.rnf identifier

instance Data.ToHeaders GetRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRule where
  toPath GetRule' {..} =
    Prelude.mconcat ["/rules/", Data.toBS identifier]

instance Data.ToQuery GetRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRuleResponse' smart constructor.
data GetRuleResponse = GetRuleResponse'
  { -- | The resource type retained by the retention rule.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The lock state for the retention rule.
    --
    -- -   @locked@ - The retention rule is locked and can\'t be modified or
    --     deleted.
    --
    -- -   @pending_unlock@ - The retention rule has been unlocked but it is
    --     still within the unlock delay period. The retention rule can be
    --     modified or deleted only after the unlock delay period has expired.
    --
    -- -   @unlocked@ - The retention rule is unlocked and it can be modified
    --     or deleted by any user with the required permissions.
    --
    -- -   @null@ - The retention rule has never been locked. Once a retention
    --     rule has been locked, it can transition between the @locked@ and
    --     @unlocked@ states only; it can never transition back to @null@.
    lockState :: Prelude.Maybe LockState,
    -- | Information about the retention rule lock configuration.
    lockConfiguration :: Prelude.Maybe LockConfiguration,
    -- | The state of the retention rule. Only retention rules that are in the
    -- @available@ state retain resources.
    status :: Prelude.Maybe RuleStatus,
    -- | Information about the resource tags used to identify resources that are
    -- retained by the retention rule.
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | The retention rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the unlock delay is set to expire. Only
    -- returned for retention rules that have been unlocked and that are still
    -- within the unlock delay period.
    lockEndTime :: Prelude.Maybe Data.POSIX,
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
-- 'lockState', 'getRuleResponse_lockState' - The lock state for the retention rule.
--
-- -   @locked@ - The retention rule is locked and can\'t be modified or
--     deleted.
--
-- -   @pending_unlock@ - The retention rule has been unlocked but it is
--     still within the unlock delay period. The retention rule can be
--     modified or deleted only after the unlock delay period has expired.
--
-- -   @unlocked@ - The retention rule is unlocked and it can be modified
--     or deleted by any user with the required permissions.
--
-- -   @null@ - The retention rule has never been locked. Once a retention
--     rule has been locked, it can transition between the @locked@ and
--     @unlocked@ states only; it can never transition back to @null@.
--
-- 'lockConfiguration', 'getRuleResponse_lockConfiguration' - Information about the retention rule lock configuration.
--
-- 'status', 'getRuleResponse_status' - The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
--
-- 'resourceTags', 'getRuleResponse_resourceTags' - Information about the resource tags used to identify resources that are
-- retained by the retention rule.
--
-- 'description', 'getRuleResponse_description' - The retention rule description.
--
-- 'lockEndTime', 'getRuleResponse_lockEndTime' - The date and time at which the unlock delay is set to expire. Only
-- returned for retention rules that have been unlocked and that are still
-- within the unlock delay period.
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
      lockState = Prelude.Nothing,
      lockConfiguration = Prelude.Nothing,
      status = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      description = Prelude.Nothing,
      lockEndTime = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      identifier = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource type retained by the retention rule.
getRuleResponse_resourceType :: Lens.Lens' GetRuleResponse (Prelude.Maybe ResourceType)
getRuleResponse_resourceType = Lens.lens (\GetRuleResponse' {resourceType} -> resourceType) (\s@GetRuleResponse' {} a -> s {resourceType = a} :: GetRuleResponse)

-- | The lock state for the retention rule.
--
-- -   @locked@ - The retention rule is locked and can\'t be modified or
--     deleted.
--
-- -   @pending_unlock@ - The retention rule has been unlocked but it is
--     still within the unlock delay period. The retention rule can be
--     modified or deleted only after the unlock delay period has expired.
--
-- -   @unlocked@ - The retention rule is unlocked and it can be modified
--     or deleted by any user with the required permissions.
--
-- -   @null@ - The retention rule has never been locked. Once a retention
--     rule has been locked, it can transition between the @locked@ and
--     @unlocked@ states only; it can never transition back to @null@.
getRuleResponse_lockState :: Lens.Lens' GetRuleResponse (Prelude.Maybe LockState)
getRuleResponse_lockState = Lens.lens (\GetRuleResponse' {lockState} -> lockState) (\s@GetRuleResponse' {} a -> s {lockState = a} :: GetRuleResponse)

-- | Information about the retention rule lock configuration.
getRuleResponse_lockConfiguration :: Lens.Lens' GetRuleResponse (Prelude.Maybe LockConfiguration)
getRuleResponse_lockConfiguration = Lens.lens (\GetRuleResponse' {lockConfiguration} -> lockConfiguration) (\s@GetRuleResponse' {} a -> s {lockConfiguration = a} :: GetRuleResponse)

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

-- | The date and time at which the unlock delay is set to expire. Only
-- returned for retention rules that have been unlocked and that are still
-- within the unlock delay period.
getRuleResponse_lockEndTime :: Lens.Lens' GetRuleResponse (Prelude.Maybe Prelude.UTCTime)
getRuleResponse_lockEndTime = Lens.lens (\GetRuleResponse' {lockEndTime} -> lockEndTime) (\s@GetRuleResponse' {} a -> s {lockEndTime = a} :: GetRuleResponse) Prelude.. Lens.mapping Data._Time

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
      `Prelude.seq` Prelude.rnf lockState
      `Prelude.seq` Prelude.rnf lockConfiguration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lockEndTime
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf httpStatus
