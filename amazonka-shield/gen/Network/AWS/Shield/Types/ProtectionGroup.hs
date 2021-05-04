{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Shield.Types.ProtectionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Shield.Types.ProtectedResourceType
import Network.AWS.Shield.Types.ProtectionGroupAggregation
import Network.AWS.Shield.Types.ProtectionGroupPattern

-- | A grouping of protected resources that you and AWS Shield Advanced can
-- monitor as a collective. This resource grouping improves the accuracy of
-- detection and reduces false positives.
--
-- /See:/ 'newProtectionGroup' smart constructor.
data ProtectionGroup = ProtectionGroup'
  { -- | The resource type to include in the protection group. All protected
    -- resources of this type are included in the protection group. You must
    -- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
    -- set it for any other @Pattern@ setting.
    resourceType :: Prelude.Maybe ProtectedResourceType,
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
    pattern' :: ProtectionGroupPattern,
    -- | The Amazon Resource Names (ARNs) of the resources to include in the
    -- protection group. You must set this when you set @Pattern@ to
    -- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
    members :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'aggregation', 'protectionGroup_aggregation' - Defines how AWS Shield combines resource data for the group in order to
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
-- 'pattern'', 'protectionGroup_pattern' - The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
--
-- 'members', 'protectionGroup_members' - The Amazon Resource Names (ARNs) of the resources to include in the
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
      { resourceType = Prelude.Nothing,
        protectionGroupId = pProtectionGroupId_,
        aggregation = pAggregation_,
        pattern' = pPattern_,
        members = Prelude.mempty
      }

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
protectionGroup_aggregation :: Lens.Lens' ProtectionGroup ProtectionGroupAggregation
protectionGroup_aggregation = Lens.lens (\ProtectionGroup' {aggregation} -> aggregation) (\s@ProtectionGroup' {} a -> s {aggregation = a} :: ProtectionGroup)

-- | The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
protectionGroup_pattern :: Lens.Lens' ProtectionGroup ProtectionGroupPattern
protectionGroup_pattern = Lens.lens (\ProtectionGroup' {pattern'} -> pattern') (\s@ProtectionGroup' {} a -> s {pattern' = a} :: ProtectionGroup)

-- | The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
protectionGroup_members :: Lens.Lens' ProtectionGroup [Prelude.Text]
protectionGroup_members = Lens.lens (\ProtectionGroup' {members} -> members) (\s@ProtectionGroup' {} a -> s {members = a} :: ProtectionGroup) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ProtectionGroup where
  parseJSON =
    Prelude.withObject
      "ProtectionGroup"
      ( \x ->
          ProtectionGroup'
            Prelude.<$> (x Prelude..:? "ResourceType")
            Prelude.<*> (x Prelude..: "ProtectionGroupId")
            Prelude.<*> (x Prelude..: "Aggregation")
            Prelude.<*> (x Prelude..: "Pattern")
            Prelude.<*> (x Prelude..:? "Members" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ProtectionGroup

instance Prelude.NFData ProtectionGroup
