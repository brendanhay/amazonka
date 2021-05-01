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
-- Module      : Network.AWS.Shield.CreateProtectionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a grouping of protected resources so they can be handled as a
-- collective. This resource grouping improves the accuracy of detection
-- and reduces false positives.
module Network.AWS.Shield.CreateProtectionGroup
  ( -- * Creating a Request
    CreateProtectionGroup (..),
    newCreateProtectionGroup,

    -- * Request Lenses
    createProtectionGroup_resourceType,
    createProtectionGroup_members,
    createProtectionGroup_protectionGroupId,
    createProtectionGroup_aggregation,
    createProtectionGroup_pattern,

    -- * Destructuring the Response
    CreateProtectionGroupResponse (..),
    newCreateProtectionGroupResponse,

    -- * Response Lenses
    createProtectionGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newCreateProtectionGroup' smart constructor.
data CreateProtectionGroup = CreateProtectionGroup'
  { -- | The resource type to include in the protection group. All protected
    -- resources of this type are included in the protection group. Newly
    -- protected resources of this type are automatically added to the group.
    -- You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you
    -- must not set it for any other @Pattern@ setting.
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
-- Create a value of 'CreateProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'createProtectionGroup_resourceType' - The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. Newly
-- protected resources of this type are automatically added to the group.
-- You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you
-- must not set it for any other @Pattern@ setting.
--
-- 'members', 'createProtectionGroup_members' - The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- 'protectionGroupId', 'createProtectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
--
-- 'aggregation', 'createProtectionGroup_aggregation' - Defines how AWS Shield combines resource data for the group in order to
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
-- 'pattern'', 'createProtectionGroup_pattern' - The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
newCreateProtectionGroup ::
  -- | 'protectionGroupId'
  Prelude.Text ->
  -- | 'aggregation'
  ProtectionGroupAggregation ->
  -- | 'pattern''
  ProtectionGroupPattern ->
  CreateProtectionGroup
newCreateProtectionGroup
  pProtectionGroupId_
  pAggregation_
  pPattern_ =
    CreateProtectionGroup'
      { resourceType =
          Prelude.Nothing,
        members = Prelude.Nothing,
        protectionGroupId = pProtectionGroupId_,
        aggregation = pAggregation_,
        pattern' = pPattern_
      }

-- | The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. Newly
-- protected resources of this type are automatically added to the group.
-- You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you
-- must not set it for any other @Pattern@ setting.
createProtectionGroup_resourceType :: Lens.Lens' CreateProtectionGroup (Prelude.Maybe ProtectedResourceType)
createProtectionGroup_resourceType = Lens.lens (\CreateProtectionGroup' {resourceType} -> resourceType) (\s@CreateProtectionGroup' {} a -> s {resourceType = a} :: CreateProtectionGroup)

-- | The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
createProtectionGroup_members :: Lens.Lens' CreateProtectionGroup (Prelude.Maybe [Prelude.Text])
createProtectionGroup_members = Lens.lens (\CreateProtectionGroup' {members} -> members) (\s@CreateProtectionGroup' {} a -> s {members = a} :: CreateProtectionGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
createProtectionGroup_protectionGroupId :: Lens.Lens' CreateProtectionGroup Prelude.Text
createProtectionGroup_protectionGroupId = Lens.lens (\CreateProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@CreateProtectionGroup' {} a -> s {protectionGroupId = a} :: CreateProtectionGroup)

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
createProtectionGroup_aggregation :: Lens.Lens' CreateProtectionGroup ProtectionGroupAggregation
createProtectionGroup_aggregation = Lens.lens (\CreateProtectionGroup' {aggregation} -> aggregation) (\s@CreateProtectionGroup' {} a -> s {aggregation = a} :: CreateProtectionGroup)

-- | The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
createProtectionGroup_pattern :: Lens.Lens' CreateProtectionGroup ProtectionGroupPattern
createProtectionGroup_pattern = Lens.lens (\CreateProtectionGroup' {pattern'} -> pattern') (\s@CreateProtectionGroup' {} a -> s {pattern' = a} :: CreateProtectionGroup)

instance Prelude.AWSRequest CreateProtectionGroup where
  type
    Rs CreateProtectionGroup =
      CreateProtectionGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateProtectionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProtectionGroup

instance Prelude.NFData CreateProtectionGroup

instance Prelude.ToHeaders CreateProtectionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.CreateProtectionGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateProtectionGroup where
  toJSON CreateProtectionGroup' {..} =
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

instance Prelude.ToPath CreateProtectionGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProtectionGroupResponse' smart constructor.
data CreateProtectionGroupResponse = CreateProtectionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProtectionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createProtectionGroupResponse_httpStatus' - The response's http status code.
newCreateProtectionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProtectionGroupResponse
newCreateProtectionGroupResponse pHttpStatus_ =
  CreateProtectionGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createProtectionGroupResponse_httpStatus :: Lens.Lens' CreateProtectionGroupResponse Prelude.Int
createProtectionGroupResponse_httpStatus = Lens.lens (\CreateProtectionGroupResponse' {httpStatus} -> httpStatus) (\s@CreateProtectionGroupResponse' {} a -> s {httpStatus = a} :: CreateProtectionGroupResponse)

instance Prelude.NFData CreateProtectionGroupResponse
