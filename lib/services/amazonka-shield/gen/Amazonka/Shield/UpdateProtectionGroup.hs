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
-- Module      : Amazonka.Shield.UpdateProtectionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing protection group. A protection group is a grouping
-- of protected resources so they can be handled as a collective. This
-- resource grouping improves the accuracy of detection and reduces false
-- positives.
module Amazonka.Shield.UpdateProtectionGroup
  ( -- * Creating a Request
    UpdateProtectionGroup (..),
    newUpdateProtectionGroup,

    -- * Request Lenses
    updateProtectionGroup_members,
    updateProtectionGroup_resourceType,
    updateProtectionGroup_protectionGroupId,
    updateProtectionGroup_aggregation,
    updateProtectionGroup_pattern,

    -- * Destructuring the Response
    UpdateProtectionGroupResponse (..),
    newUpdateProtectionGroupResponse,

    -- * Response Lenses
    updateProtectionGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newUpdateProtectionGroup' smart constructor.
data UpdateProtectionGroup = UpdateProtectionGroup'
  { -- | The Amazon Resource Names (ARNs) of the resources to include in the
    -- protection group. You must set this when you set @Pattern@ to
    -- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
    members :: Prelude.Maybe [Prelude.Text],
    -- | The resource type to include in the protection group. All protected
    -- resources of this type are included in the protection group. You must
    -- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
    -- set it for any other @Pattern@ setting.
    resourceType :: Prelude.Maybe ProtectedResourceType,
    -- | The name of the protection group. You use this to identify the
    -- protection group in lists and to manage the protection group, for
    -- example to update, delete, or describe it.
    protectionGroupId :: Prelude.Text,
    -- | Defines how Shield combines resource data for the group in order to
    -- detect, mitigate, and report events.
    --
    -- -   Sum - Use the total traffic across the group. This is a good choice
    --     for most cases. Examples include Elastic IP addresses for EC2
    --     instances that scale manually or automatically.
    --
    -- -   Mean - Use the average of the traffic across the group. This is a
    --     good choice for resources that share traffic uniformly. Examples
    --     include accelerators and load balancers.
    --
    -- -   Max - Use the highest traffic from each resource. This is useful for
    --     resources that don\'t share traffic and for resources that share
    --     that traffic in a non-uniform way. Examples include Amazon
    --     CloudFront distributions and origin resources for CloudFront
    --     distributions.
    aggregation :: ProtectionGroupAggregation,
    -- | The criteria to use to choose the protected resources for inclusion in
    -- the group. You can include all resources that have protections, provide
    -- a list of resource Amazon Resource Names (ARNs), or include all
    -- resources of a specified resource type.
    pattern' :: ProtectionGroupPattern
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'members', 'updateProtectionGroup_members' - The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- 'resourceType', 'updateProtectionGroup_resourceType' - The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. You must
-- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
-- set it for any other @Pattern@ setting.
--
-- 'protectionGroupId', 'updateProtectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
--
-- 'aggregation', 'updateProtectionGroup_aggregation' - Defines how Shield combines resource data for the group in order to
-- detect, mitigate, and report events.
--
-- -   Sum - Use the total traffic across the group. This is a good choice
--     for most cases. Examples include Elastic IP addresses for EC2
--     instances that scale manually or automatically.
--
-- -   Mean - Use the average of the traffic across the group. This is a
--     good choice for resources that share traffic uniformly. Examples
--     include accelerators and load balancers.
--
-- -   Max - Use the highest traffic from each resource. This is useful for
--     resources that don\'t share traffic and for resources that share
--     that traffic in a non-uniform way. Examples include Amazon
--     CloudFront distributions and origin resources for CloudFront
--     distributions.
--
-- 'pattern'', 'updateProtectionGroup_pattern' - The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
newUpdateProtectionGroup ::
  -- | 'protectionGroupId'
  Prelude.Text ->
  -- | 'aggregation'
  ProtectionGroupAggregation ->
  -- | 'pattern''
  ProtectionGroupPattern ->
  UpdateProtectionGroup
newUpdateProtectionGroup
  pProtectionGroupId_
  pAggregation_
  pPattern_ =
    UpdateProtectionGroup'
      { members = Prelude.Nothing,
        resourceType = Prelude.Nothing,
        protectionGroupId = pProtectionGroupId_,
        aggregation = pAggregation_,
        pattern' = pPattern_
      }

-- | The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
updateProtectionGroup_members :: Lens.Lens' UpdateProtectionGroup (Prelude.Maybe [Prelude.Text])
updateProtectionGroup_members = Lens.lens (\UpdateProtectionGroup' {members} -> members) (\s@UpdateProtectionGroup' {} a -> s {members = a} :: UpdateProtectionGroup) Prelude.. Lens.mapping Lens.coerced

-- | The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. You must
-- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
-- set it for any other @Pattern@ setting.
updateProtectionGroup_resourceType :: Lens.Lens' UpdateProtectionGroup (Prelude.Maybe ProtectedResourceType)
updateProtectionGroup_resourceType = Lens.lens (\UpdateProtectionGroup' {resourceType} -> resourceType) (\s@UpdateProtectionGroup' {} a -> s {resourceType = a} :: UpdateProtectionGroup)

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
updateProtectionGroup_protectionGroupId :: Lens.Lens' UpdateProtectionGroup Prelude.Text
updateProtectionGroup_protectionGroupId = Lens.lens (\UpdateProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@UpdateProtectionGroup' {} a -> s {protectionGroupId = a} :: UpdateProtectionGroup)

-- | Defines how Shield combines resource data for the group in order to
-- detect, mitigate, and report events.
--
-- -   Sum - Use the total traffic across the group. This is a good choice
--     for most cases. Examples include Elastic IP addresses for EC2
--     instances that scale manually or automatically.
--
-- -   Mean - Use the average of the traffic across the group. This is a
--     good choice for resources that share traffic uniformly. Examples
--     include accelerators and load balancers.
--
-- -   Max - Use the highest traffic from each resource. This is useful for
--     resources that don\'t share traffic and for resources that share
--     that traffic in a non-uniform way. Examples include Amazon
--     CloudFront distributions and origin resources for CloudFront
--     distributions.
updateProtectionGroup_aggregation :: Lens.Lens' UpdateProtectionGroup ProtectionGroupAggregation
updateProtectionGroup_aggregation = Lens.lens (\UpdateProtectionGroup' {aggregation} -> aggregation) (\s@UpdateProtectionGroup' {} a -> s {aggregation = a} :: UpdateProtectionGroup)

-- | The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
updateProtectionGroup_pattern :: Lens.Lens' UpdateProtectionGroup ProtectionGroupPattern
updateProtectionGroup_pattern = Lens.lens (\UpdateProtectionGroup' {pattern'} -> pattern') (\s@UpdateProtectionGroup' {} a -> s {pattern' = a} :: UpdateProtectionGroup)

instance Core.AWSRequest UpdateProtectionGroup where
  type
    AWSResponse UpdateProtectionGroup =
      UpdateProtectionGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProtectionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProtectionGroup where
  hashWithSalt _salt UpdateProtectionGroup' {..} =
    _salt
      `Prelude.hashWithSalt` members
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` protectionGroupId
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` pattern'

instance Prelude.NFData UpdateProtectionGroup where
  rnf UpdateProtectionGroup' {..} =
    Prelude.rnf members `Prelude.seq`
      Prelude.rnf resourceType `Prelude.seq`
        Prelude.rnf protectionGroupId `Prelude.seq`
          Prelude.rnf aggregation `Prelude.seq`
            Prelude.rnf pattern'

instance Data.ToHeaders UpdateProtectionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.UpdateProtectionGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProtectionGroup where
  toJSON UpdateProtectionGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Members" Data..=) Prelude.<$> members,
            ("ResourceType" Data..=) Prelude.<$> resourceType,
            Prelude.Just
              ("ProtectionGroupId" Data..= protectionGroupId),
            Prelude.Just ("Aggregation" Data..= aggregation),
            Prelude.Just ("Pattern" Data..= pattern')
          ]
      )

instance Data.ToPath UpdateProtectionGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProtectionGroupResponse' smart constructor.
data UpdateProtectionGroupResponse = UpdateProtectionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProtectionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProtectionGroupResponse_httpStatus' - The response's http status code.
newUpdateProtectionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProtectionGroupResponse
newUpdateProtectionGroupResponse pHttpStatus_ =
  UpdateProtectionGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateProtectionGroupResponse_httpStatus :: Lens.Lens' UpdateProtectionGroupResponse Prelude.Int
updateProtectionGroupResponse_httpStatus = Lens.lens (\UpdateProtectionGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateProtectionGroupResponse' {} a -> s {httpStatus = a} :: UpdateProtectionGroupResponse)

instance Prelude.NFData UpdateProtectionGroupResponse where
  rnf UpdateProtectionGroupResponse' {..} =
    Prelude.rnf httpStatus
