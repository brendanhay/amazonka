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
-- Module      : Amazonka.RBin.LockRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Locks a retention rule. A locked retention rule can\'t be modified or
-- deleted.
module Amazonka.RBin.LockRule
  ( -- * Creating a Request
    LockRule (..),
    newLockRule,

    -- * Request Lenses
    lockRule_identifier,
    lockRule_lockConfiguration,

    -- * Destructuring the Response
    LockRuleResponse (..),
    newLockRuleResponse,

    -- * Response Lenses
    lockRuleResponse_description,
    lockRuleResponse_identifier,
    lockRuleResponse_lockConfiguration,
    lockRuleResponse_lockState,
    lockRuleResponse_resourceTags,
    lockRuleResponse_resourceType,
    lockRuleResponse_retentionPeriod,
    lockRuleResponse_status,
    lockRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newLockRule' smart constructor.
data LockRule = LockRule'
  { -- | The unique ID of the retention rule.
    identifier :: Prelude.Text,
    -- | Information about the retention rule lock configuration.
    lockConfiguration :: LockConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LockRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'lockRule_identifier' - The unique ID of the retention rule.
--
-- 'lockConfiguration', 'lockRule_lockConfiguration' - Information about the retention rule lock configuration.
newLockRule ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'lockConfiguration'
  LockConfiguration ->
  LockRule
newLockRule pIdentifier_ pLockConfiguration_ =
  LockRule'
    { identifier = pIdentifier_,
      lockConfiguration = pLockConfiguration_
    }

-- | The unique ID of the retention rule.
lockRule_identifier :: Lens.Lens' LockRule Prelude.Text
lockRule_identifier = Lens.lens (\LockRule' {identifier} -> identifier) (\s@LockRule' {} a -> s {identifier = a} :: LockRule)

-- | Information about the retention rule lock configuration.
lockRule_lockConfiguration :: Lens.Lens' LockRule LockConfiguration
lockRule_lockConfiguration = Lens.lens (\LockRule' {lockConfiguration} -> lockConfiguration) (\s@LockRule' {} a -> s {lockConfiguration = a} :: LockRule)

instance Core.AWSRequest LockRule where
  type AWSResponse LockRule = LockRuleResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          LockRuleResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Identifier")
            Prelude.<*> (x Data..?> "LockConfiguration")
            Prelude.<*> (x Data..?> "LockState")
            Prelude.<*> (x Data..?> "ResourceTags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ResourceType")
            Prelude.<*> (x Data..?> "RetentionPeriod")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LockRule where
  hashWithSalt _salt LockRule' {..} =
    _salt
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` lockConfiguration

instance Prelude.NFData LockRule where
  rnf LockRule' {..} =
    Prelude.rnf identifier `Prelude.seq`
      Prelude.rnf lockConfiguration

instance Data.ToHeaders LockRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON LockRule where
  toJSON LockRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LockConfiguration" Data..= lockConfiguration)
          ]
      )

instance Data.ToPath LockRule where
  toPath LockRule' {..} =
    Prelude.mconcat
      ["/rules/", Data.toBS identifier, "/lock"]

instance Data.ToQuery LockRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newLockRuleResponse' smart constructor.
data LockRuleResponse = LockRuleResponse'
  { -- | The retention rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the retention rule.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | Information about the retention rule lock configuration.
    lockConfiguration :: Prelude.Maybe LockConfiguration,
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
    -- | Information about the resource tags used to identify resources that are
    -- retained by the retention rule.
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | The resource type retained by the retention rule.
    resourceType :: Prelude.Maybe ResourceType,
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The state of the retention rule. Only retention rules that are in the
    -- @available@ state retain resources.
    status :: Prelude.Maybe RuleStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LockRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'lockRuleResponse_description' - The retention rule description.
--
-- 'identifier', 'lockRuleResponse_identifier' - The unique ID of the retention rule.
--
-- 'lockConfiguration', 'lockRuleResponse_lockConfiguration' - Information about the retention rule lock configuration.
--
-- 'lockState', 'lockRuleResponse_lockState' - The lock state for the retention rule.
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
-- 'resourceTags', 'lockRuleResponse_resourceTags' - Information about the resource tags used to identify resources that are
-- retained by the retention rule.
--
-- 'resourceType', 'lockRuleResponse_resourceType' - The resource type retained by the retention rule.
--
-- 'retentionPeriod', 'lockRuleResponse_retentionPeriod' - Undocumented member.
--
-- 'status', 'lockRuleResponse_status' - The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
--
-- 'httpStatus', 'lockRuleResponse_httpStatus' - The response's http status code.
newLockRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  LockRuleResponse
newLockRuleResponse pHttpStatus_ =
  LockRuleResponse'
    { description = Prelude.Nothing,
      identifier = Prelude.Nothing,
      lockConfiguration = Prelude.Nothing,
      lockState = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The retention rule description.
lockRuleResponse_description :: Lens.Lens' LockRuleResponse (Prelude.Maybe Prelude.Text)
lockRuleResponse_description = Lens.lens (\LockRuleResponse' {description} -> description) (\s@LockRuleResponse' {} a -> s {description = a} :: LockRuleResponse)

-- | The unique ID of the retention rule.
lockRuleResponse_identifier :: Lens.Lens' LockRuleResponse (Prelude.Maybe Prelude.Text)
lockRuleResponse_identifier = Lens.lens (\LockRuleResponse' {identifier} -> identifier) (\s@LockRuleResponse' {} a -> s {identifier = a} :: LockRuleResponse)

-- | Information about the retention rule lock configuration.
lockRuleResponse_lockConfiguration :: Lens.Lens' LockRuleResponse (Prelude.Maybe LockConfiguration)
lockRuleResponse_lockConfiguration = Lens.lens (\LockRuleResponse' {lockConfiguration} -> lockConfiguration) (\s@LockRuleResponse' {} a -> s {lockConfiguration = a} :: LockRuleResponse)

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
lockRuleResponse_lockState :: Lens.Lens' LockRuleResponse (Prelude.Maybe LockState)
lockRuleResponse_lockState = Lens.lens (\LockRuleResponse' {lockState} -> lockState) (\s@LockRuleResponse' {} a -> s {lockState = a} :: LockRuleResponse)

-- | Information about the resource tags used to identify resources that are
-- retained by the retention rule.
lockRuleResponse_resourceTags :: Lens.Lens' LockRuleResponse (Prelude.Maybe [ResourceTag])
lockRuleResponse_resourceTags = Lens.lens (\LockRuleResponse' {resourceTags} -> resourceTags) (\s@LockRuleResponse' {} a -> s {resourceTags = a} :: LockRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The resource type retained by the retention rule.
lockRuleResponse_resourceType :: Lens.Lens' LockRuleResponse (Prelude.Maybe ResourceType)
lockRuleResponse_resourceType = Lens.lens (\LockRuleResponse' {resourceType} -> resourceType) (\s@LockRuleResponse' {} a -> s {resourceType = a} :: LockRuleResponse)

-- | Undocumented member.
lockRuleResponse_retentionPeriod :: Lens.Lens' LockRuleResponse (Prelude.Maybe RetentionPeriod)
lockRuleResponse_retentionPeriod = Lens.lens (\LockRuleResponse' {retentionPeriod} -> retentionPeriod) (\s@LockRuleResponse' {} a -> s {retentionPeriod = a} :: LockRuleResponse)

-- | The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
lockRuleResponse_status :: Lens.Lens' LockRuleResponse (Prelude.Maybe RuleStatus)
lockRuleResponse_status = Lens.lens (\LockRuleResponse' {status} -> status) (\s@LockRuleResponse' {} a -> s {status = a} :: LockRuleResponse)

-- | The response's http status code.
lockRuleResponse_httpStatus :: Lens.Lens' LockRuleResponse Prelude.Int
lockRuleResponse_httpStatus = Lens.lens (\LockRuleResponse' {httpStatus} -> httpStatus) (\s@LockRuleResponse' {} a -> s {httpStatus = a} :: LockRuleResponse)

instance Prelude.NFData LockRuleResponse where
  rnf LockRuleResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf identifier `Prelude.seq`
        Prelude.rnf lockConfiguration `Prelude.seq`
          Prelude.rnf lockState `Prelude.seq`
            Prelude.rnf resourceTags `Prelude.seq`
              Prelude.rnf resourceType `Prelude.seq`
                Prelude.rnf retentionPeriod `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf httpStatus
