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
-- Module      : Amazonka.RBin.UnlockRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlocks a retention rule. After a retention rule is unlocked, it can be
-- modified or deleted only after the unlock delay period expires.
module Amazonka.RBin.UnlockRule
  ( -- * Creating a Request
    UnlockRule (..),
    newUnlockRule,

    -- * Request Lenses
    unlockRule_identifier,

    -- * Destructuring the Response
    UnlockRuleResponse (..),
    newUnlockRuleResponse,

    -- * Response Lenses
    unlockRuleResponse_resourceType,
    unlockRuleResponse_lockState,
    unlockRuleResponse_lockConfiguration,
    unlockRuleResponse_status,
    unlockRuleResponse_resourceTags,
    unlockRuleResponse_description,
    unlockRuleResponse_lockEndTime,
    unlockRuleResponse_retentionPeriod,
    unlockRuleResponse_identifier,
    unlockRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnlockRule' smart constructor.
data UnlockRule = UnlockRule'
  { -- | The unique ID of the retention rule.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlockRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'unlockRule_identifier' - The unique ID of the retention rule.
newUnlockRule ::
  -- | 'identifier'
  Prelude.Text ->
  UnlockRule
newUnlockRule pIdentifier_ =
  UnlockRule' {identifier = pIdentifier_}

-- | The unique ID of the retention rule.
unlockRule_identifier :: Lens.Lens' UnlockRule Prelude.Text
unlockRule_identifier = Lens.lens (\UnlockRule' {identifier} -> identifier) (\s@UnlockRule' {} a -> s {identifier = a} :: UnlockRule)

instance Core.AWSRequest UnlockRule where
  type AWSResponse UnlockRule = UnlockRuleResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UnlockRuleResponse'
            Prelude.<$> (x Core..?> "ResourceType")
            Prelude.<*> (x Core..?> "LockState")
            Prelude.<*> (x Core..?> "LockConfiguration")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ResourceTags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "LockEndTime")
            Prelude.<*> (x Core..?> "RetentionPeriod")
            Prelude.<*> (x Core..?> "Identifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnlockRule where
  hashWithSalt _salt UnlockRule' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData UnlockRule where
  rnf UnlockRule' {..} = Prelude.rnf identifier

instance Core.ToHeaders UnlockRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UnlockRule where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath UnlockRule where
  toPath UnlockRule' {..} =
    Prelude.mconcat
      ["/rules/", Core.toBS identifier, "/unlock"]

instance Core.ToQuery UnlockRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnlockRuleResponse' smart constructor.
data UnlockRuleResponse = UnlockRuleResponse'
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
    lockEndTime :: Prelude.Maybe Core.POSIX,
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The unique ID of the retention rule.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlockRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'unlockRuleResponse_resourceType' - The resource type retained by the retention rule.
--
-- 'lockState', 'unlockRuleResponse_lockState' - The lock state for the retention rule.
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
-- 'lockConfiguration', 'unlockRuleResponse_lockConfiguration' - Information about the retention rule lock configuration.
--
-- 'status', 'unlockRuleResponse_status' - The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
--
-- 'resourceTags', 'unlockRuleResponse_resourceTags' - Information about the resource tags used to identify resources that are
-- retained by the retention rule.
--
-- 'description', 'unlockRuleResponse_description' - The retention rule description.
--
-- 'lockEndTime', 'unlockRuleResponse_lockEndTime' - The date and time at which the unlock delay is set to expire. Only
-- returned for retention rules that have been unlocked and that are still
-- within the unlock delay period.
--
-- 'retentionPeriod', 'unlockRuleResponse_retentionPeriod' - Undocumented member.
--
-- 'identifier', 'unlockRuleResponse_identifier' - The unique ID of the retention rule.
--
-- 'httpStatus', 'unlockRuleResponse_httpStatus' - The response's http status code.
newUnlockRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnlockRuleResponse
newUnlockRuleResponse pHttpStatus_ =
  UnlockRuleResponse'
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
unlockRuleResponse_resourceType :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe ResourceType)
unlockRuleResponse_resourceType = Lens.lens (\UnlockRuleResponse' {resourceType} -> resourceType) (\s@UnlockRuleResponse' {} a -> s {resourceType = a} :: UnlockRuleResponse)

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
unlockRuleResponse_lockState :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe LockState)
unlockRuleResponse_lockState = Lens.lens (\UnlockRuleResponse' {lockState} -> lockState) (\s@UnlockRuleResponse' {} a -> s {lockState = a} :: UnlockRuleResponse)

-- | Information about the retention rule lock configuration.
unlockRuleResponse_lockConfiguration :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe LockConfiguration)
unlockRuleResponse_lockConfiguration = Lens.lens (\UnlockRuleResponse' {lockConfiguration} -> lockConfiguration) (\s@UnlockRuleResponse' {} a -> s {lockConfiguration = a} :: UnlockRuleResponse)

-- | The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
unlockRuleResponse_status :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe RuleStatus)
unlockRuleResponse_status = Lens.lens (\UnlockRuleResponse' {status} -> status) (\s@UnlockRuleResponse' {} a -> s {status = a} :: UnlockRuleResponse)

-- | Information about the resource tags used to identify resources that are
-- retained by the retention rule.
unlockRuleResponse_resourceTags :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe [ResourceTag])
unlockRuleResponse_resourceTags = Lens.lens (\UnlockRuleResponse' {resourceTags} -> resourceTags) (\s@UnlockRuleResponse' {} a -> s {resourceTags = a} :: UnlockRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The retention rule description.
unlockRuleResponse_description :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe Prelude.Text)
unlockRuleResponse_description = Lens.lens (\UnlockRuleResponse' {description} -> description) (\s@UnlockRuleResponse' {} a -> s {description = a} :: UnlockRuleResponse)

-- | The date and time at which the unlock delay is set to expire. Only
-- returned for retention rules that have been unlocked and that are still
-- within the unlock delay period.
unlockRuleResponse_lockEndTime :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe Prelude.UTCTime)
unlockRuleResponse_lockEndTime = Lens.lens (\UnlockRuleResponse' {lockEndTime} -> lockEndTime) (\s@UnlockRuleResponse' {} a -> s {lockEndTime = a} :: UnlockRuleResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
unlockRuleResponse_retentionPeriod :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe RetentionPeriod)
unlockRuleResponse_retentionPeriod = Lens.lens (\UnlockRuleResponse' {retentionPeriod} -> retentionPeriod) (\s@UnlockRuleResponse' {} a -> s {retentionPeriod = a} :: UnlockRuleResponse)

-- | The unique ID of the retention rule.
unlockRuleResponse_identifier :: Lens.Lens' UnlockRuleResponse (Prelude.Maybe Prelude.Text)
unlockRuleResponse_identifier = Lens.lens (\UnlockRuleResponse' {identifier} -> identifier) (\s@UnlockRuleResponse' {} a -> s {identifier = a} :: UnlockRuleResponse)

-- | The response's http status code.
unlockRuleResponse_httpStatus :: Lens.Lens' UnlockRuleResponse Prelude.Int
unlockRuleResponse_httpStatus = Lens.lens (\UnlockRuleResponse' {httpStatus} -> httpStatus) (\s@UnlockRuleResponse' {} a -> s {httpStatus = a} :: UnlockRuleResponse)

instance Prelude.NFData UnlockRuleResponse where
  rnf UnlockRuleResponse' {..} =
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
