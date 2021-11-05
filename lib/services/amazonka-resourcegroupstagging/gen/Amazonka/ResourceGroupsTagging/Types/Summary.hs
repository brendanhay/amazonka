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
-- Module      : Amazonka.ResourceGroupsTagging.Types.Summary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroupsTagging.Types.Summary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroupsTagging.Types.TargetIdType

-- | A count of noncompliant resources.
--
-- /See:/ 'newSummary' smart constructor.
data Summary = Summary'
  { -- | The account identifier or the root identifier of the organization. If
    -- you don\'t know the root ID, you can call the AWS Organizations
    -- <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots>
    -- API.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that shows when this summary was generated in this Region.
    lastUpdated :: Prelude.Maybe Prelude.Text,
    -- | The AWS resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The count of noncompliant resources.
    nonCompliantResources :: Prelude.Maybe Prelude.Integer,
    -- | Whether the target is an account, an OU, or the organization root.
    targetIdType :: Prelude.Maybe TargetIdType,
    -- | The AWS Region that the summary applies to.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'lastUpdated', 'summary_lastUpdated' - The timestamp that shows when this summary was generated in this Region.
--
-- 'resourceType', 'summary_resourceType' - The AWS resource type.
--
-- 'nonCompliantResources', 'summary_nonCompliantResources' - The count of noncompliant resources.
--
-- 'targetIdType', 'summary_targetIdType' - Whether the target is an account, an OU, or the organization root.
--
-- 'region', 'summary_region' - The AWS Region that the summary applies to.
newSummary ::
  Summary
newSummary =
  Summary'
    { targetId = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      nonCompliantResources = Prelude.Nothing,
      targetIdType = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The account identifier or the root identifier of the organization. If
-- you don\'t know the root ID, you can call the AWS Organizations
-- <http://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots>
-- API.
summary_targetId :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_targetId = Lens.lens (\Summary' {targetId} -> targetId) (\s@Summary' {} a -> s {targetId = a} :: Summary)

-- | The timestamp that shows when this summary was generated in this Region.
summary_lastUpdated :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_lastUpdated = Lens.lens (\Summary' {lastUpdated} -> lastUpdated) (\s@Summary' {} a -> s {lastUpdated = a} :: Summary)

-- | The AWS resource type.
summary_resourceType :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_resourceType = Lens.lens (\Summary' {resourceType} -> resourceType) (\s@Summary' {} a -> s {resourceType = a} :: Summary)

-- | The count of noncompliant resources.
summary_nonCompliantResources :: Lens.Lens' Summary (Prelude.Maybe Prelude.Integer)
summary_nonCompliantResources = Lens.lens (\Summary' {nonCompliantResources} -> nonCompliantResources) (\s@Summary' {} a -> s {nonCompliantResources = a} :: Summary)

-- | Whether the target is an account, an OU, or the organization root.
summary_targetIdType :: Lens.Lens' Summary (Prelude.Maybe TargetIdType)
summary_targetIdType = Lens.lens (\Summary' {targetIdType} -> targetIdType) (\s@Summary' {} a -> s {targetIdType = a} :: Summary)

-- | The AWS Region that the summary applies to.
summary_region :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_region = Lens.lens (\Summary' {region} -> region) (\s@Summary' {} a -> s {region = a} :: Summary)

instance Core.FromJSON Summary where
  parseJSON =
    Core.withObject
      "Summary"
      ( \x ->
          Summary'
            Prelude.<$> (x Core..:? "TargetId")
            Prelude.<*> (x Core..:? "LastUpdated")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "NonCompliantResources")
            Prelude.<*> (x Core..:? "TargetIdType")
            Prelude.<*> (x Core..:? "Region")
      )

instance Prelude.Hashable Summary

instance Prelude.NFData Summary
