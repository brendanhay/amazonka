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
-- Module      : Amazonka.Shield.CreateProtectionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a grouping of protected resources so they can be handled as a
-- collective. This resource grouping improves the accuracy of detection
-- and reduces false positives.
module Amazonka.Shield.CreateProtectionGroup
  ( -- * Creating a Request
    CreateProtectionGroup (..),
    newCreateProtectionGroup,

    -- * Request Lenses
    createProtectionGroup_members,
    createProtectionGroup_resourceType,
    createProtectionGroup_tags,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newCreateProtectionGroup' smart constructor.
data CreateProtectionGroup = CreateProtectionGroup'
  { -- | The Amazon Resource Names (ARNs) of the resources to include in the
    -- protection group. You must set this when you set @Pattern@ to
    -- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
    members :: Prelude.Maybe [Prelude.Text],
    -- | The resource type to include in the protection group. All protected
    -- resources of this type are included in the protection group. Newly
    -- protected resources of this type are automatically added to the group.
    -- You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you
    -- must not set it for any other @Pattern@ setting.
    resourceType :: Prelude.Maybe ProtectedResourceType,
    -- | One or more tag key-value pairs for the protection group.
    tags :: Prelude.Maybe [Tag],
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
    --     CloudFront and origin resources for CloudFront distributions.
    aggregation :: ProtectionGroupAggregation,
    -- | The criteria to use to choose the protected resources for inclusion in
    -- the group. You can include all resources that have protections, provide
    -- a list of resource Amazon Resource Names (ARNs), or include all
    -- resources of a specified resource type.
    pattern' :: ProtectionGroupPattern
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'members', 'createProtectionGroup_members' - The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
--
-- 'resourceType', 'createProtectionGroup_resourceType' - The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. Newly
-- protected resources of this type are automatically added to the group.
-- You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you
-- must not set it for any other @Pattern@ setting.
--
-- 'tags', 'createProtectionGroup_tags' - One or more tag key-value pairs for the protection group.
--
-- 'protectionGroupId', 'createProtectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
--
-- 'aggregation', 'createProtectionGroup_aggregation' - Defines how Shield combines resource data for the group in order to
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
--     CloudFront and origin resources for CloudFront distributions.
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
      { members = Prelude.Nothing,
        resourceType = Prelude.Nothing,
        tags = Prelude.Nothing,
        protectionGroupId = pProtectionGroupId_,
        aggregation = pAggregation_,
        pattern' = pPattern_
      }

-- | The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
createProtectionGroup_members :: Lens.Lens' CreateProtectionGroup (Prelude.Maybe [Prelude.Text])
createProtectionGroup_members = Lens.lens (\CreateProtectionGroup' {members} -> members) (\s@CreateProtectionGroup' {} a -> s {members = a} :: CreateProtectionGroup) Prelude.. Lens.mapping Lens.coerced

-- | The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. Newly
-- protected resources of this type are automatically added to the group.
-- You must set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you
-- must not set it for any other @Pattern@ setting.
createProtectionGroup_resourceType :: Lens.Lens' CreateProtectionGroup (Prelude.Maybe ProtectedResourceType)
createProtectionGroup_resourceType = Lens.lens (\CreateProtectionGroup' {resourceType} -> resourceType) (\s@CreateProtectionGroup' {} a -> s {resourceType = a} :: CreateProtectionGroup)

-- | One or more tag key-value pairs for the protection group.
createProtectionGroup_tags :: Lens.Lens' CreateProtectionGroup (Prelude.Maybe [Tag])
createProtectionGroup_tags = Lens.lens (\CreateProtectionGroup' {tags} -> tags) (\s@CreateProtectionGroup' {} a -> s {tags = a} :: CreateProtectionGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
createProtectionGroup_protectionGroupId :: Lens.Lens' CreateProtectionGroup Prelude.Text
createProtectionGroup_protectionGroupId = Lens.lens (\CreateProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@CreateProtectionGroup' {} a -> s {protectionGroupId = a} :: CreateProtectionGroup)

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
--     CloudFront and origin resources for CloudFront distributions.
createProtectionGroup_aggregation :: Lens.Lens' CreateProtectionGroup ProtectionGroupAggregation
createProtectionGroup_aggregation = Lens.lens (\CreateProtectionGroup' {aggregation} -> aggregation) (\s@CreateProtectionGroup' {} a -> s {aggregation = a} :: CreateProtectionGroup)

-- | The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
createProtectionGroup_pattern :: Lens.Lens' CreateProtectionGroup ProtectionGroupPattern
createProtectionGroup_pattern = Lens.lens (\CreateProtectionGroup' {pattern'} -> pattern') (\s@CreateProtectionGroup' {} a -> s {pattern' = a} :: CreateProtectionGroup)

instance Core.AWSRequest CreateProtectionGroup where
  type
    AWSResponse CreateProtectionGroup =
      CreateProtectionGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateProtectionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProtectionGroup where
  hashWithSalt _salt CreateProtectionGroup' {..} =
    _salt
      `Prelude.hashWithSalt` members
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` protectionGroupId
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` pattern'

instance Prelude.NFData CreateProtectionGroup where
  rnf CreateProtectionGroup' {..} =
    Prelude.rnf members `Prelude.seq`
      Prelude.rnf resourceType `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf protectionGroupId `Prelude.seq`
            Prelude.rnf aggregation `Prelude.seq`
              Prelude.rnf pattern'

instance Data.ToHeaders CreateProtectionGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.CreateProtectionGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProtectionGroup where
  toJSON CreateProtectionGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Members" Data..=) Prelude.<$> members,
            ("ResourceType" Data..=) Prelude.<$> resourceType,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ProtectionGroupId" Data..= protectionGroupId),
            Prelude.Just ("Aggregation" Data..= aggregation),
            Prelude.Just ("Pattern" Data..= pattern')
          ]
      )

instance Data.ToPath CreateProtectionGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateProtectionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProtectionGroupResponse' smart constructor.
data CreateProtectionGroupResponse = CreateProtectionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateProtectionGroupResponse where
  rnf CreateProtectionGroupResponse' {..} =
    Prelude.rnf httpStatus
