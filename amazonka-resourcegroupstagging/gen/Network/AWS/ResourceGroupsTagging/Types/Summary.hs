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
-- Module      : Network.AWS.ResourceGroupsTagging.Types.Summary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.Summary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroupsTagging.Types.TargetIdType

-- | A count of noncompliant resources.
--
-- /See:/ 'newSummary' smart constructor.
data Summary = Summary'
  { -- | The account identifier or the root identifier of the organization. If
    -- you don\'t know the root ID, you can call the AWS Organizations
    -- <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots>
    -- API.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The AWS resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that shows when this summary was generated in this Region.
    lastUpdated :: Prelude.Maybe Prelude.Text,
    -- | Whether the target is an account, an OU, or the organization root.
    targetIdType :: Prelude.Maybe TargetIdType,
    -- | The count of noncompliant resources.
    nonCompliantResources :: Prelude.Maybe Prelude.Integer,
    -- | The AWS Region that the summary applies to.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Summary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetId', 'summary_targetId' - The account identifier or the root identifier of the organization. If
-- you don\'t know the root ID, you can call the AWS Organizations
-- <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots>
-- API.
--
-- 'resourceType', 'summary_resourceType' - The AWS resource type.
--
-- 'lastUpdated', 'summary_lastUpdated' - The timestamp that shows when this summary was generated in this Region.
--
-- 'targetIdType', 'summary_targetIdType' - Whether the target is an account, an OU, or the organization root.
--
-- 'nonCompliantResources', 'summary_nonCompliantResources' - The count of noncompliant resources.
--
-- 'region', 'summary_region' - The AWS Region that the summary applies to.
newSummary ::
  Summary
newSummary =
  Summary'
    { targetId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      targetIdType = Prelude.Nothing,
      nonCompliantResources = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The account identifier or the root identifier of the organization. If
-- you don\'t know the root ID, you can call the AWS Organizations
-- <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots>
-- API.
summary_targetId :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_targetId = Lens.lens (\Summary' {targetId} -> targetId) (\s@Summary' {} a -> s {targetId = a} :: Summary)

-- | The AWS resource type.
summary_resourceType :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_resourceType = Lens.lens (\Summary' {resourceType} -> resourceType) (\s@Summary' {} a -> s {resourceType = a} :: Summary)

-- | The timestamp that shows when this summary was generated in this Region.
summary_lastUpdated :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_lastUpdated = Lens.lens (\Summary' {lastUpdated} -> lastUpdated) (\s@Summary' {} a -> s {lastUpdated = a} :: Summary)

-- | Whether the target is an account, an OU, or the organization root.
summary_targetIdType :: Lens.Lens' Summary (Prelude.Maybe TargetIdType)
summary_targetIdType = Lens.lens (\Summary' {targetIdType} -> targetIdType) (\s@Summary' {} a -> s {targetIdType = a} :: Summary)

-- | The count of noncompliant resources.
summary_nonCompliantResources :: Lens.Lens' Summary (Prelude.Maybe Prelude.Integer)
summary_nonCompliantResources = Lens.lens (\Summary' {nonCompliantResources} -> nonCompliantResources) (\s@Summary' {} a -> s {nonCompliantResources = a} :: Summary)

-- | The AWS Region that the summary applies to.
summary_region :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_region = Lens.lens (\Summary' {region} -> region) (\s@Summary' {} a -> s {region = a} :: Summary)

instance Prelude.FromJSON Summary where
  parseJSON =
    Prelude.withObject
      "Summary"
      ( \x ->
          Summary'
            Prelude.<$> (x Prelude..:? "TargetId")
            Prelude.<*> (x Prelude..:? "ResourceType")
            Prelude.<*> (x Prelude..:? "LastUpdated")
            Prelude.<*> (x Prelude..:? "TargetIdType")
            Prelude.<*> (x Prelude..:? "NonCompliantResources")
            Prelude.<*> (x Prelude..:? "Region")
      )

instance Prelude.Hashable Summary

instance Prelude.NFData Summary
