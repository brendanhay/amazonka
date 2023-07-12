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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroupsTagging.Types.Summary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroupsTagging.Types.TargetIdType

-- | A count of noncompliant resources.
--
-- /See:/ 'newSummary' smart constructor.
data Summary = Summary'
  { -- | The timestamp that shows when this summary was generated in this Region.
    lastUpdated :: Prelude.Maybe Prelude.Text,
    -- | The count of noncompliant resources.
    nonCompliantResources :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Web Services Region that the summary applies to.
    region :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The account identifier or the root identifier of the organization. If
    -- you don\'t know the root ID, you can call the Organizations
    -- <https://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots>
    -- API.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | Whether the target is an account, an OU, or the organization root.
    targetIdType :: Prelude.Maybe TargetIdType
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
-- 'lastUpdated', 'summary_lastUpdated' - The timestamp that shows when this summary was generated in this Region.
--
-- 'nonCompliantResources', 'summary_nonCompliantResources' - The count of noncompliant resources.
--
-- 'region', 'summary_region' - The Amazon Web Services Region that the summary applies to.
--
-- 'resourceType', 'summary_resourceType' - The Amazon Web Services resource type.
--
-- 'targetId', 'summary_targetId' - The account identifier or the root identifier of the organization. If
-- you don\'t know the root ID, you can call the Organizations
-- <https://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots>
-- API.
--
-- 'targetIdType', 'summary_targetIdType' - Whether the target is an account, an OU, or the organization root.
newSummary ::
  Summary
newSummary =
  Summary'
    { lastUpdated = Prelude.Nothing,
      nonCompliantResources = Prelude.Nothing,
      region = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      targetId = Prelude.Nothing,
      targetIdType = Prelude.Nothing
    }

-- | The timestamp that shows when this summary was generated in this Region.
summary_lastUpdated :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_lastUpdated = Lens.lens (\Summary' {lastUpdated} -> lastUpdated) (\s@Summary' {} a -> s {lastUpdated = a} :: Summary)

-- | The count of noncompliant resources.
summary_nonCompliantResources :: Lens.Lens' Summary (Prelude.Maybe Prelude.Integer)
summary_nonCompliantResources = Lens.lens (\Summary' {nonCompliantResources} -> nonCompliantResources) (\s@Summary' {} a -> s {nonCompliantResources = a} :: Summary)

-- | The Amazon Web Services Region that the summary applies to.
summary_region :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_region = Lens.lens (\Summary' {region} -> region) (\s@Summary' {} a -> s {region = a} :: Summary)

-- | The Amazon Web Services resource type.
summary_resourceType :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_resourceType = Lens.lens (\Summary' {resourceType} -> resourceType) (\s@Summary' {} a -> s {resourceType = a} :: Summary)

-- | The account identifier or the root identifier of the organization. If
-- you don\'t know the root ID, you can call the Organizations
-- <https://docs.aws.amazon.com/organizations/latest/APIReference/API_ListRoots.html ListRoots>
-- API.
summary_targetId :: Lens.Lens' Summary (Prelude.Maybe Prelude.Text)
summary_targetId = Lens.lens (\Summary' {targetId} -> targetId) (\s@Summary' {} a -> s {targetId = a} :: Summary)

-- | Whether the target is an account, an OU, or the organization root.
summary_targetIdType :: Lens.Lens' Summary (Prelude.Maybe TargetIdType)
summary_targetIdType = Lens.lens (\Summary' {targetIdType} -> targetIdType) (\s@Summary' {} a -> s {targetIdType = a} :: Summary)

instance Data.FromJSON Summary where
  parseJSON =
    Data.withObject
      "Summary"
      ( \x ->
          Summary'
            Prelude.<$> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "NonCompliantResources")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "TargetId")
            Prelude.<*> (x Data..:? "TargetIdType")
      )

instance Prelude.Hashable Summary where
  hashWithSalt _salt Summary' {..} =
    _salt
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` nonCompliantResources
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` targetIdType

instance Prelude.NFData Summary where
  rnf Summary' {..} =
    Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf nonCompliantResources
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf targetIdType
