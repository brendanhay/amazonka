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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ESInstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the Amazon OpenSearch Service instances that Amazon Web
-- Services recommends that you purchase.
--
-- /See:/ 'newESInstanceDetails' smart constructor.
data ESInstanceDetails = ESInstanceDetails'
  { -- | Determines whether the recommendation is for a current-generation
    -- instance.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | The class of instance that Amazon Web Services recommends.
    instanceClass :: Prelude.Maybe Prelude.Text,
    -- | The size of instance that Amazon Web Services recommends.
    instanceSize :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the recommended reservation.
    region :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the recommended reservation is size flexible.
    sizeFlexEligible :: Prelude.Maybe Prelude.Bool
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
-- 'currentGeneration', 'eSInstanceDetails_currentGeneration' - Determines whether the recommendation is for a current-generation
-- instance.
--
-- 'instanceClass', 'eSInstanceDetails_instanceClass' - The class of instance that Amazon Web Services recommends.
--
-- 'instanceSize', 'eSInstanceDetails_instanceSize' - The size of instance that Amazon Web Services recommends.
--
-- 'region', 'eSInstanceDetails_region' - The Amazon Web Services Region of the recommended reservation.
--
-- 'sizeFlexEligible', 'eSInstanceDetails_sizeFlexEligible' - Determines whether the recommended reservation is size flexible.
newESInstanceDetails ::
  ESInstanceDetails
newESInstanceDetails =
  ESInstanceDetails'
    { currentGeneration =
        Prelude.Nothing,
      instanceClass = Prelude.Nothing,
      instanceSize = Prelude.Nothing,
      region = Prelude.Nothing,
      sizeFlexEligible = Prelude.Nothing
    }

-- | Determines whether the recommendation is for a current-generation
-- instance.
eSInstanceDetails_currentGeneration :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Bool)
eSInstanceDetails_currentGeneration = Lens.lens (\ESInstanceDetails' {currentGeneration} -> currentGeneration) (\s@ESInstanceDetails' {} a -> s {currentGeneration = a} :: ESInstanceDetails)

-- | The class of instance that Amazon Web Services recommends.
eSInstanceDetails_instanceClass :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Text)
eSInstanceDetails_instanceClass = Lens.lens (\ESInstanceDetails' {instanceClass} -> instanceClass) (\s@ESInstanceDetails' {} a -> s {instanceClass = a} :: ESInstanceDetails)

-- | The size of instance that Amazon Web Services recommends.
eSInstanceDetails_instanceSize :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Text)
eSInstanceDetails_instanceSize = Lens.lens (\ESInstanceDetails' {instanceSize} -> instanceSize) (\s@ESInstanceDetails' {} a -> s {instanceSize = a} :: ESInstanceDetails)

-- | The Amazon Web Services Region of the recommended reservation.
eSInstanceDetails_region :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Text)
eSInstanceDetails_region = Lens.lens (\ESInstanceDetails' {region} -> region) (\s@ESInstanceDetails' {} a -> s {region = a} :: ESInstanceDetails)

-- | Determines whether the recommended reservation is size flexible.
eSInstanceDetails_sizeFlexEligible :: Lens.Lens' ESInstanceDetails (Prelude.Maybe Prelude.Bool)
eSInstanceDetails_sizeFlexEligible = Lens.lens (\ESInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@ESInstanceDetails' {} a -> s {sizeFlexEligible = a} :: ESInstanceDetails)

instance Data.FromJSON ESInstanceDetails where
  parseJSON =
    Data.withObject
      "ESInstanceDetails"
      ( \x ->
          ESInstanceDetails'
            Prelude.<$> (x Data..:? "CurrentGeneration")
            Prelude.<*> (x Data..:? "InstanceClass")
            Prelude.<*> (x Data..:? "InstanceSize")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "SizeFlexEligible")
      )

instance Prelude.Hashable ESInstanceDetails where
  hashWithSalt _salt ESInstanceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` currentGeneration
      `Prelude.hashWithSalt` instanceClass
      `Prelude.hashWithSalt` instanceSize
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` sizeFlexEligible

instance Prelude.NFData ESInstanceDetails where
  rnf ESInstanceDetails' {..} =
    Prelude.rnf currentGeneration `Prelude.seq`
      Prelude.rnf instanceClass `Prelude.seq`
        Prelude.rnf instanceSize `Prelude.seq`
          Prelude.rnf region `Prelude.seq`
            Prelude.rnf sizeFlexEligible
