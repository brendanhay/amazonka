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
-- Module      : Amazonka.DrS.Types.SourceCloudProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.SourceCloudProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties of the cloud environment where this Source Server originated
-- from.
--
-- /See:/ 'newSourceCloudProperties' smart constructor.
data SourceCloudProperties = SourceCloudProperties'
  { -- | AWS Account ID for an EC2-originated Source Server.
    originAccountID :: Prelude.Maybe Prelude.Text,
    -- | AWS Availability Zone for an EC2-originated Source Server.
    originAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | AWS Region for an EC2-originated Source Server.
    originRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceCloudProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originAccountID', 'sourceCloudProperties_originAccountID' - AWS Account ID for an EC2-originated Source Server.
--
-- 'originAvailabilityZone', 'sourceCloudProperties_originAvailabilityZone' - AWS Availability Zone for an EC2-originated Source Server.
--
-- 'originRegion', 'sourceCloudProperties_originRegion' - AWS Region for an EC2-originated Source Server.
newSourceCloudProperties ::
  SourceCloudProperties
newSourceCloudProperties =
  SourceCloudProperties'
    { originAccountID =
        Prelude.Nothing,
      originAvailabilityZone = Prelude.Nothing,
      originRegion = Prelude.Nothing
    }

-- | AWS Account ID for an EC2-originated Source Server.
sourceCloudProperties_originAccountID :: Lens.Lens' SourceCloudProperties (Prelude.Maybe Prelude.Text)
sourceCloudProperties_originAccountID = Lens.lens (\SourceCloudProperties' {originAccountID} -> originAccountID) (\s@SourceCloudProperties' {} a -> s {originAccountID = a} :: SourceCloudProperties)

-- | AWS Availability Zone for an EC2-originated Source Server.
sourceCloudProperties_originAvailabilityZone :: Lens.Lens' SourceCloudProperties (Prelude.Maybe Prelude.Text)
sourceCloudProperties_originAvailabilityZone = Lens.lens (\SourceCloudProperties' {originAvailabilityZone} -> originAvailabilityZone) (\s@SourceCloudProperties' {} a -> s {originAvailabilityZone = a} :: SourceCloudProperties)

-- | AWS Region for an EC2-originated Source Server.
sourceCloudProperties_originRegion :: Lens.Lens' SourceCloudProperties (Prelude.Maybe Prelude.Text)
sourceCloudProperties_originRegion = Lens.lens (\SourceCloudProperties' {originRegion} -> originRegion) (\s@SourceCloudProperties' {} a -> s {originRegion = a} :: SourceCloudProperties)

instance Data.FromJSON SourceCloudProperties where
  parseJSON =
    Data.withObject
      "SourceCloudProperties"
      ( \x ->
          SourceCloudProperties'
            Prelude.<$> (x Data..:? "originAccountID")
            Prelude.<*> (x Data..:? "originAvailabilityZone")
            Prelude.<*> (x Data..:? "originRegion")
      )

instance Prelude.Hashable SourceCloudProperties where
  hashWithSalt _salt SourceCloudProperties' {..} =
    _salt `Prelude.hashWithSalt` originAccountID
      `Prelude.hashWithSalt` originAvailabilityZone
      `Prelude.hashWithSalt` originRegion

instance Prelude.NFData SourceCloudProperties where
  rnf SourceCloudProperties' {..} =
    Prelude.rnf originAccountID
      `Prelude.seq` Prelude.rnf originAvailabilityZone
      `Prelude.seq` Prelude.rnf originRegion
