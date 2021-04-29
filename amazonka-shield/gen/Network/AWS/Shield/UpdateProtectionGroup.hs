{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.UpdateProtectionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing protection group. A protection group is a grouping
-- of protected resources so they can be handled as a collective. This
-- resource grouping improves the accuracy of detection and reduces false
-- positives.
module Network.AWS.Shield.UpdateProtectionGroup
  ( -- * Creating a Request
    UpdateProtectionGroup (..),
    newUpdateProtectionGroup,

    -- * Request Lenses
    updateProtectionGroup_resourceType,
    updateProtectionGroup_members,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newUpdateProtectionGroup' smart constructor.
data UpdateProtectionGroup = UpdateProtectionGroup'
  { -- | The resource type to include in the protection group. All protected
    -- resources of this type are included in the protection group. You must
    -- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
    -- set it for any other @Pattern@ setting.
    resourceType :: Prelude.Maybe ProtectedResourceType,
    -- | The Amazon Resource Names (ARNs) of the resources to include in the
    -- protection group. You must set this when you set @Pattern@ to
    -- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
    members :: Prelude.Maybe [Prelude.Text],
    -- | The name of the protection group. You use this to identify the
    -- protection group in lists and to manage the protection group, for
    -- example to update, delete, or describe it.
    protectionGroupId :: Prelude.Text,
    -- | Defines how AWS Shield combines resource data for the group in order to
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
    --     that traffic in a non-uniform way. Examples include CloudFront
    --     distributions and origin resources for CloudFront distributions.
    aggregation :: ProtectionGroupAggregation,
    -- | The criteria to use to choose the protected resources for inclusion in
    -- the group. You can include all resources that have protections, provide
    -- a list of resource Amazon Resource Names (ARNs), or include all
    -- resources of a specified resource type.
    pattern' :: ProtectionGroupPattern
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'updateProtectionGroup_resourceType' - The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. You must
-- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
-- set it for any other @Pattern@ setting.
--
-- 'members', 'updateProtectionGroup_members' - The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- 'protectionGroupId', 'updateProtectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
--
-- 'aggregation', 'updateProtectionGroup_aggregation' - Defines how AWS Shield combines resource data for the group in order to
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
--     that traffic in a non-uniform way. Examples include CloudFront
--     distributions and origin resources for CloudFront distributions.
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
      { resourceType =
          Prelude.Nothing,
        members = Prelude.Nothing,
        protectionGroupId = pProtectionGroupId_,
        aggregation = pAggregation_,
        pattern' = pPattern_
      }

-- | The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. You must
-- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
-- set it for any other @Pattern@ setting.
updateProtectionGroup_resourceType :: Lens.Lens' UpdateProtectionGroup (Prelude.Maybe ProtectedResourceType)
updateProtectionGroup_resourceType = Lens.lens (\UpdateProtectionGroup' {resourceType} -> resourceType) (\s@UpdateProtectionGroup' {} a -> s {resourceType = a} :: UpdateProtectionGroup)

-- | The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
updateProtectionGroup_members :: Lens.Lens' UpdateProtectionGroup (Prelude.Maybe [Prelude.Text])
updateProtectionGroup_members = Lens.lens (\UpdateProtectionGroup' {members} -> members) (\s@UpdateProtectionGroup' {} a -> s {members = a} :: UpdateProtectionGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
updateProtectionGroup_protectionGroupId :: Lens.Lens' UpdateProtectionGroup Prelude.Text
updateProtectionGroup_protectionGroupId = Lens.lens (\UpdateProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@UpdateProtectionGroup' {} a -> s {protectionGroupId = a} :: UpdateProtectionGroup)

-- | Defines how AWS Shield combines resource data for the group in order to
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
--     that traffic in a non-uniform way. Examples include CloudFront
--     distributions and origin resources for CloudFront distributions.
updateProtectionGroup_aggregation :: Lens.Lens' UpdateProtectionGroup ProtectionGroupAggregation
updateProtectionGroup_aggregation = Lens.lens (\UpdateProtectionGroup' {aggregation} -> aggregation) (\s@UpdateProtectionGroup' {} a -> s {aggregation = a} :: UpdateProtectionGroup)

-- | The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
updateProtectionGroup_pattern :: Lens.Lens' UpdateProtectionGroup ProtectionGroupPattern
updateProtectionGroup_pattern = Lens.lens (\UpdateProtectionGroup' {pattern'} -> pattern') (\s@UpdateProtectionGroup' {} a -> s {pattern' = a} :: UpdateProtectionGroup)

instance Prelude.AWSRequest UpdateProtectionGroup where
  type
    Rs UpdateProtectionGroup =
      UpdateProtectionGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProtectionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProtectionGroup

instance Prelude.NFData UpdateProtectionGroup

instance Prelude.ToHeaders UpdateProtectionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.UpdateProtectionGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateProtectionGroup where
  toJSON UpdateProtectionGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceType" Prelude..=)
              Prelude.<$> resourceType,
            ("Members" Prelude..=) Prelude.<$> members,
            Prelude.Just
              ("ProtectionGroupId" Prelude..= protectionGroupId),
            Prelude.Just ("Aggregation" Prelude..= aggregation),
            Prelude.Just ("Pattern" Prelude..= pattern')
          ]
      )

instance Prelude.ToPath UpdateProtectionGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProtectionGroupResponse' smart constructor.
data UpdateProtectionGroupResponse = UpdateProtectionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData UpdateProtectionGroupResponse
