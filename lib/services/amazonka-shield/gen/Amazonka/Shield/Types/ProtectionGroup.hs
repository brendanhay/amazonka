{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Shield.Types.ProtectionGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProtectionGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ProtectedResourceType
import Amazonka.Shield.Types.ProtectionGroupAggregation
import Amazonka.Shield.Types.ProtectionGroupPattern

-- | A grouping of protected resources that you and Shield Advanced can
-- monitor as a collective. This resource grouping improves the accuracy of
-- detection and reduces false positives.
--
-- /See:/ 'newProtectionGroup' smart constructor.
data ProtectionGroup = ProtectionGroup'
  { -- | The ARN (Amazon Resource Name) of the protection group.
    protectionGroupArn :: Prelude.Maybe Prelude.Text,
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
    -- a list of resource ARNs (Amazon Resource Names), or include all
    -- resources of a specified resource type.
    pattern' :: ProtectionGroupPattern,
    -- | The ARNs (Amazon Resource Names) of the resources to include in the
    -- protection group. You must set this when you set @Pattern@ to
    -- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
    members :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionGroupArn', 'protectionGroup_protectionGroupArn' - The ARN (Amazon Resource Name) of the protection group.
--
-- 'resourceType', 'protectionGroup_resourceType' - The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. You must
-- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
-- set it for any other @Pattern@ setting.
--
-- 'protectionGroupId', 'protectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
--
-- 'aggregation', 'protectionGroup_aggregation' - Defines how Shield combines resource data for the group in order to
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
-- 'pattern'', 'protectionGroup_pattern' - The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource ARNs (Amazon Resource Names), or include all
-- resources of a specified resource type.
--
-- 'members', 'protectionGroup_members' - The ARNs (Amazon Resource Names) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
newProtectionGroup ::
  -- | 'protectionGroupId'
  Prelude.Text ->
  -- | 'aggregation'
  ProtectionGroupAggregation ->
  -- | 'pattern''
  ProtectionGroupPattern ->
  ProtectionGroup
newProtectionGroup
  pProtectionGroupId_
  pAggregation_
  pPattern_ =
    ProtectionGroup'
      { protectionGroupArn =
          Prelude.Nothing,
        resourceType = Prelude.Nothing,
        protectionGroupId = pProtectionGroupId_,
        aggregation = pAggregation_,
        pattern' = pPattern_,
        members = Prelude.mempty
      }

-- | The ARN (Amazon Resource Name) of the protection group.
protectionGroup_protectionGroupArn :: Lens.Lens' ProtectionGroup (Prelude.Maybe Prelude.Text)
protectionGroup_protectionGroupArn = Lens.lens (\ProtectionGroup' {protectionGroupArn} -> protectionGroupArn) (\s@ProtectionGroup' {} a -> s {protectionGroupArn = a} :: ProtectionGroup)

-- | The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. You must
-- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
-- set it for any other @Pattern@ setting.
protectionGroup_resourceType :: Lens.Lens' ProtectionGroup (Prelude.Maybe ProtectedResourceType)
protectionGroup_resourceType = Lens.lens (\ProtectionGroup' {resourceType} -> resourceType) (\s@ProtectionGroup' {} a -> s {resourceType = a} :: ProtectionGroup)

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
protectionGroup_protectionGroupId :: Lens.Lens' ProtectionGroup Prelude.Text
protectionGroup_protectionGroupId = Lens.lens (\ProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@ProtectionGroup' {} a -> s {protectionGroupId = a} :: ProtectionGroup)

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
protectionGroup_aggregation :: Lens.Lens' ProtectionGroup ProtectionGroupAggregation
protectionGroup_aggregation = Lens.lens (\ProtectionGroup' {aggregation} -> aggregation) (\s@ProtectionGroup' {} a -> s {aggregation = a} :: ProtectionGroup)

-- | The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource ARNs (Amazon Resource Names), or include all
-- resources of a specified resource type.
protectionGroup_pattern :: Lens.Lens' ProtectionGroup ProtectionGroupPattern
protectionGroup_pattern = Lens.lens (\ProtectionGroup' {pattern'} -> pattern') (\s@ProtectionGroup' {} a -> s {pattern' = a} :: ProtectionGroup)

-- | The ARNs (Amazon Resource Names) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
protectionGroup_members :: Lens.Lens' ProtectionGroup [Prelude.Text]
protectionGroup_members = Lens.lens (\ProtectionGroup' {members} -> members) (\s@ProtectionGroup' {} a -> s {members = a} :: ProtectionGroup) Prelude.. Lens.coerced

instance Data.FromJSON ProtectionGroup where
  parseJSON =
    Data.withObject
      "ProtectionGroup"
      ( \x ->
          ProtectionGroup'
            Prelude.<$> (x Data..:? "ProtectionGroupArn")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..: "ProtectionGroupId")
            Prelude.<*> (x Data..: "Aggregation")
            Prelude.<*> (x Data..: "Pattern")
            Prelude.<*> (x Data..:? "Members" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ProtectionGroup where
  hashWithSalt _salt ProtectionGroup' {..} =
    _salt `Prelude.hashWithSalt` protectionGroupArn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` protectionGroupId
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` pattern'
      `Prelude.hashWithSalt` members

instance Prelude.NFData ProtectionGroup where
  rnf ProtectionGroup' {..} =
    Prelude.rnf protectionGroupArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf protectionGroupId
      `Prelude.seq` Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf pattern'
      `Prelude.seq` Prelude.rnf members
