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
-- Module      : Amazonka.CostExplorer.Types.ESInstanceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ESInstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about the Amazon OpenSearch Service instances that Amazon Web
-- Services recommends that you purchase.
--
-- /See:/ 'newESInstanceDetails' smart constructor.
data ESInstanceDetails = ESInstanceDetails'
  { -- | Determines whether the recommended reservation is size flexible.
    sizeFlexEligible :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services Region of the recommended reservation.
    region :: Prelude.Maybe Prelude.Text,
    -- | The class of instance that Amazon Web Services recommends.
    instanceClass :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the recommendation is for a current-generation
    -- instance.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | The size of instance that Amazon Web Services recommends.
    instanceSize :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ESInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeFlexEligible', 'eSInstanceDetails_sizeFlexEligible' - Determines whether the recommended reservation is size flexible.
--
-- 'region', 'eSInstanceDetails_region' - The Amazon Web Services Region of the recommended reservation.
--
-- 'instanceClass', 'eSInstanceDetails_instanceClass' - The class of instance that Amazon Web Services recommends.
--
-- 'currentGeneration', 'eSInstanceDetails_currentGeneration' - Determines whether the recommendation is for a current-generation
-- instance.
--
-- 'instanceSize', 'eSInstanceDetails_instanceSize' - The size of instance that Amazon Web Services recommends.
newESInstanceDetails ::
  ESInstanceDetails
newESInstanceDetails =
  ESInstanceDetails'
    { sizeFlexEligible =
        Prelude.Nothing,
      region = Prelude.Nothing,
      instanceClass = Prelude.Nothing,
      currentGeneration = Prelude.Nothing,
      instanceSize = Prelude.Nothing
    }

-- | Determines whether the recommended reservation is size flexible.
eSInstanceDetails_sizeFlexEligible :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Bool)
eSInstanceDetails_sizeFlexEligible = Lens.lens (\ESInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@ESInstanceDetails' {} a -> s {sizeFlexEligible = a} :: ESInstanceDetails)

-- | The Amazon Web Services Region of the recommended reservation.
eSInstanceDetails_region :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Text)
eSInstanceDetails_region = Lens.lens (\ESInstanceDetails' {region} -> region) (\s@ESInstanceDetails' {} a -> s {region = a} :: ESInstanceDetails)

-- | The class of instance that Amazon Web Services recommends.
eSInstanceDetails_instanceClass :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Text)
eSInstanceDetails_instanceClass = Lens.lens (\ESInstanceDetails' {instanceClass} -> instanceClass) (\s@ESInstanceDetails' {} a -> s {instanceClass = a} :: ESInstanceDetails)

-- | Determines whether the recommendation is for a current-generation
-- instance.
eSInstanceDetails_currentGeneration :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Bool)
eSInstanceDetails_currentGeneration = Lens.lens (\ESInstanceDetails' {currentGeneration} -> currentGeneration) (\s@ESInstanceDetails' {} a -> s {currentGeneration = a} :: ESInstanceDetails)

-- | The size of instance that Amazon Web Services recommends.
eSInstanceDetails_instanceSize :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Text)
eSInstanceDetails_instanceSize = Lens.lens (\ESInstanceDetails' {instanceSize} -> instanceSize) (\s@ESInstanceDetails' {} a -> s {instanceSize = a} :: ESInstanceDetails)

instance Core.FromJSON ESInstanceDetails where
  parseJSON =
    Core.withObject
      "ESInstanceDetails"
      ( \x ->
          ESInstanceDetails'
            Prelude.<$> (x Core..:? "SizeFlexEligible")
            Prelude.<*> (x Core..:? "Region")
            Prelude.<*> (x Core..:? "InstanceClass")
            Prelude.<*> (x Core..:? "CurrentGeneration")
            Prelude.<*> (x Core..:? "InstanceSize")
      )

instance Prelude.Hashable ESInstanceDetails where
  hashWithSalt _salt ESInstanceDetails' {..} =
    _salt `Prelude.hashWithSalt` sizeFlexEligible
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` instanceClass
      `Prelude.hashWithSalt` currentGeneration
      `Prelude.hashWithSalt` instanceSize

instance Prelude.NFData ESInstanceDetails where
  rnf ESInstanceDetails' {..} =
    Prelude.rnf sizeFlexEligible
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf instanceClass
      `Prelude.seq` Prelude.rnf currentGeneration
      `Prelude.seq` Prelude.rnf instanceSize
