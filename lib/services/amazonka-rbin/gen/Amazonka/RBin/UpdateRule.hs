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
-- Module      : Amazonka.RBin.UpdateRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Recycle Bin retention rule. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/recycle-bin-working-with-rules.html#recycle-bin-update-rule Update Recycle Bin retention rules>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.RBin.UpdateRule
  ( -- * Creating a Request
    UpdateRule (..),
    newUpdateRule,

    -- * Request Lenses
    updateRule_resourceType,
    updateRule_resourceTags,
    updateRule_description,
    updateRule_retentionPeriod,
    updateRule_identifier,

    -- * Destructuring the Response
    UpdateRuleResponse (..),
    newUpdateRuleResponse,

    -- * Response Lenses
    updateRuleResponse_resourceType,
    updateRuleResponse_status,
    updateRuleResponse_resourceTags,
    updateRuleResponse_description,
    updateRuleResponse_retentionPeriod,
    updateRuleResponse_identifier,
    updateRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRule' smart constructor.
data UpdateRule = UpdateRule'
  { -- | The resource type to be retained by the retention rule. Currently, only
    -- Amazon EBS snapshots and EBS-backed AMIs are supported. To retain
    -- snapshots, specify @EBS_SNAPSHOT@. To retain EBS-backed AMIs, specify
    -- @EC2_IMAGE@.
    resourceType :: Prelude.Maybe ResourceType,
    -- | Specifies the resource tags to use to identify resources that are to be
    -- retained by a tag-level retention rule. For tag-level retention rules,
    -- only deleted resources, of the specified resource type, that have one or
    -- more of the specified tag key and value pairs are retained. If a
    -- resource is deleted, but it does not have any of the specified tag key
    -- and value pairs, it is immediately deleted without being retained by the
    -- retention rule.
    --
    -- You can add the same tag key and value pair to a maximum or five
    -- retention rules.
    --
    -- To create a Region-level retention rule, omit this parameter. A
    -- Region-level retention rule does not have any resource tags specified.
    -- It retains all deleted resources of the specified resource type in the
    -- Region in which the rule is created, even if the resources are not
    -- tagged.
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | The retention rule description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the retention period for which the retention rule is
    -- to retain resources.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The unique ID of the retention rule.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'updateRule_resourceType' - The resource type to be retained by the retention rule. Currently, only
-- Amazon EBS snapshots and EBS-backed AMIs are supported. To retain
-- snapshots, specify @EBS_SNAPSHOT@. To retain EBS-backed AMIs, specify
-- @EC2_IMAGE@.
--
-- 'resourceTags', 'updateRule_resourceTags' - Specifies the resource tags to use to identify resources that are to be
-- retained by a tag-level retention rule. For tag-level retention rules,
-- only deleted resources, of the specified resource type, that have one or
-- more of the specified tag key and value pairs are retained. If a
-- resource is deleted, but it does not have any of the specified tag key
-- and value pairs, it is immediately deleted without being retained by the
-- retention rule.
--
-- You can add the same tag key and value pair to a maximum or five
-- retention rules.
--
-- To create a Region-level retention rule, omit this parameter. A
-- Region-level retention rule does not have any resource tags specified.
-- It retains all deleted resources of the specified resource type in the
-- Region in which the rule is created, even if the resources are not
-- tagged.
--
-- 'description', 'updateRule_description' - The retention rule description.
--
-- 'retentionPeriod', 'updateRule_retentionPeriod' - Information about the retention period for which the retention rule is
-- to retain resources.
--
-- 'identifier', 'updateRule_identifier' - The unique ID of the retention rule.
newUpdateRule ::
  -- | 'identifier'
  Prelude.Text ->
  UpdateRule
newUpdateRule pIdentifier_ =
  UpdateRule'
    { resourceType = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      description = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | The resource type to be retained by the retention rule. Currently, only
-- Amazon EBS snapshots and EBS-backed AMIs are supported. To retain
-- snapshots, specify @EBS_SNAPSHOT@. To retain EBS-backed AMIs, specify
-- @EC2_IMAGE@.
updateRule_resourceType :: Lens.Lens' UpdateRule (Prelude.Maybe ResourceType)
updateRule_resourceType = Lens.lens (\UpdateRule' {resourceType} -> resourceType) (\s@UpdateRule' {} a -> s {resourceType = a} :: UpdateRule)

-- | Specifies the resource tags to use to identify resources that are to be
-- retained by a tag-level retention rule. For tag-level retention rules,
-- only deleted resources, of the specified resource type, that have one or
-- more of the specified tag key and value pairs are retained. If a
-- resource is deleted, but it does not have any of the specified tag key
-- and value pairs, it is immediately deleted without being retained by the
-- retention rule.
--
-- You can add the same tag key and value pair to a maximum or five
-- retention rules.
--
-- To create a Region-level retention rule, omit this parameter. A
-- Region-level retention rule does not have any resource tags specified.
-- It retains all deleted resources of the specified resource type in the
-- Region in which the rule is created, even if the resources are not
-- tagged.
updateRule_resourceTags :: Lens.Lens' UpdateRule (Prelude.Maybe [ResourceTag])
updateRule_resourceTags = Lens.lens (\UpdateRule' {resourceTags} -> resourceTags) (\s@UpdateRule' {} a -> s {resourceTags = a} :: UpdateRule) Prelude.. Lens.mapping Lens.coerced

-- | The retention rule description.
updateRule_description :: Lens.Lens' UpdateRule (Prelude.Maybe Prelude.Text)
updateRule_description = Lens.lens (\UpdateRule' {description} -> description) (\s@UpdateRule' {} a -> s {description = a} :: UpdateRule)

-- | Information about the retention period for which the retention rule is
-- to retain resources.
updateRule_retentionPeriod :: Lens.Lens' UpdateRule (Prelude.Maybe RetentionPeriod)
updateRule_retentionPeriod = Lens.lens (\UpdateRule' {retentionPeriod} -> retentionPeriod) (\s@UpdateRule' {} a -> s {retentionPeriod = a} :: UpdateRule)

-- | The unique ID of the retention rule.
updateRule_identifier :: Lens.Lens' UpdateRule Prelude.Text
updateRule_identifier = Lens.lens (\UpdateRule' {identifier} -> identifier) (\s@UpdateRule' {} a -> s {identifier = a} :: UpdateRule)

instance Core.AWSRequest UpdateRule where
  type AWSResponse UpdateRule = UpdateRuleResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleResponse'
            Prelude.<$> (x Core..?> "ResourceType")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "ResourceTags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RetentionPeriod")
            Prelude.<*> (x Core..?> "Identifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRule where
  hashWithSalt _salt UpdateRule' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData UpdateRule where
  rnf UpdateRule' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf identifier

instance Core.ToHeaders UpdateRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRule where
  toJSON UpdateRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceType" Core..=) Prelude.<$> resourceType,
            ("ResourceTags" Core..=) Prelude.<$> resourceTags,
            ("Description" Core..=) Prelude.<$> description,
            ("RetentionPeriod" Core..=)
              Prelude.<$> retentionPeriod
          ]
      )

instance Core.ToPath UpdateRule where
  toPath UpdateRule' {..} =
    Prelude.mconcat ["/rules/", Core.toBS identifier]

instance Core.ToQuery UpdateRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleResponse' smart constructor.
data UpdateRuleResponse = UpdateRuleResponse'
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
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The unique ID of the retention rule.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'updateRuleResponse_resourceType' - The resource type retained by the retention rule.
--
-- 'status', 'updateRuleResponse_status' - The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
--
-- 'resourceTags', 'updateRuleResponse_resourceTags' - Information about the resource tags used to identify resources that are
-- retained by the retention rule.
--
-- 'description', 'updateRuleResponse_description' - The retention rule description.
--
-- 'retentionPeriod', 'updateRuleResponse_retentionPeriod' - Undocumented member.
--
-- 'identifier', 'updateRuleResponse_identifier' - The unique ID of the retention rule.
--
-- 'httpStatus', 'updateRuleResponse_httpStatus' - The response's http status code.
newUpdateRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRuleResponse
newUpdateRuleResponse pHttpStatus_ =
  UpdateRuleResponse'
    { resourceType = Prelude.Nothing,
      status = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      description = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      identifier = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource type retained by the retention rule.
updateRuleResponse_resourceType :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe ResourceType)
updateRuleResponse_resourceType = Lens.lens (\UpdateRuleResponse' {resourceType} -> resourceType) (\s@UpdateRuleResponse' {} a -> s {resourceType = a} :: UpdateRuleResponse)

-- | The state of the retention rule. Only retention rules that are in the
-- @available@ state retain resources.
updateRuleResponse_status :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe RuleStatus)
updateRuleResponse_status = Lens.lens (\UpdateRuleResponse' {status} -> status) (\s@UpdateRuleResponse' {} a -> s {status = a} :: UpdateRuleResponse)

-- | Information about the resource tags used to identify resources that are
-- retained by the retention rule.
updateRuleResponse_resourceTags :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe [ResourceTag])
updateRuleResponse_resourceTags = Lens.lens (\UpdateRuleResponse' {resourceTags} -> resourceTags) (\s@UpdateRuleResponse' {} a -> s {resourceTags = a} :: UpdateRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The retention rule description.
updateRuleResponse_description :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe Prelude.Text)
updateRuleResponse_description = Lens.lens (\UpdateRuleResponse' {description} -> description) (\s@UpdateRuleResponse' {} a -> s {description = a} :: UpdateRuleResponse)

-- | Undocumented member.
updateRuleResponse_retentionPeriod :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe RetentionPeriod)
updateRuleResponse_retentionPeriod = Lens.lens (\UpdateRuleResponse' {retentionPeriod} -> retentionPeriod) (\s@UpdateRuleResponse' {} a -> s {retentionPeriod = a} :: UpdateRuleResponse)

-- | The unique ID of the retention rule.
updateRuleResponse_identifier :: Lens.Lens' UpdateRuleResponse (Prelude.Maybe Prelude.Text)
updateRuleResponse_identifier = Lens.lens (\UpdateRuleResponse' {identifier} -> identifier) (\s@UpdateRuleResponse' {} a -> s {identifier = a} :: UpdateRuleResponse)

-- | The response's http status code.
updateRuleResponse_httpStatus :: Lens.Lens' UpdateRuleResponse Prelude.Int
updateRuleResponse_httpStatus = Lens.lens (\UpdateRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleResponse' {} a -> s {httpStatus = a} :: UpdateRuleResponse)

instance Prelude.NFData UpdateRuleResponse where
  rnf UpdateRuleResponse' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf httpStatus
