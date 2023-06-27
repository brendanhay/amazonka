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
-- Module      : Amazonka.Evidently.Types.OnlineAbDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.OnlineAbDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration of which variation to use as
-- the \"control\" version. The \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
--
-- /See:/ 'newOnlineAbDefinition' smart constructor.
data OnlineAbDefinition = OnlineAbDefinition'
  { -- | The name of the variation that is the default variation that the other
    -- variations are compared to.
    controlTreatmentName :: Prelude.Maybe Prelude.Text,
    -- | A set of key-value pairs. The keys are variation names, and the values
    -- are the portion of experiment traffic to be assigned to that variation.
    -- The traffic portion is specified in thousandths of a percent, so 20,000
    -- for a variation would allocate 20% of the experiment traffic to that
    -- variation.
    treatmentWeights :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnlineAbDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlTreatmentName', 'onlineAbDefinition_controlTreatmentName' - The name of the variation that is the default variation that the other
-- variations are compared to.
--
-- 'treatmentWeights', 'onlineAbDefinition_treatmentWeights' - A set of key-value pairs. The keys are variation names, and the values
-- are the portion of experiment traffic to be assigned to that variation.
-- The traffic portion is specified in thousandths of a percent, so 20,000
-- for a variation would allocate 20% of the experiment traffic to that
-- variation.
newOnlineAbDefinition ::
  OnlineAbDefinition
newOnlineAbDefinition =
  OnlineAbDefinition'
    { controlTreatmentName =
        Prelude.Nothing,
      treatmentWeights = Prelude.Nothing
    }

-- | The name of the variation that is the default variation that the other
-- variations are compared to.
onlineAbDefinition_controlTreatmentName :: Lens.Lens' OnlineAbDefinition (Prelude.Maybe Prelude.Text)
onlineAbDefinition_controlTreatmentName = Lens.lens (\OnlineAbDefinition' {controlTreatmentName} -> controlTreatmentName) (\s@OnlineAbDefinition' {} a -> s {controlTreatmentName = a} :: OnlineAbDefinition)

-- | A set of key-value pairs. The keys are variation names, and the values
-- are the portion of experiment traffic to be assigned to that variation.
-- The traffic portion is specified in thousandths of a percent, so 20,000
-- for a variation would allocate 20% of the experiment traffic to that
-- variation.
onlineAbDefinition_treatmentWeights :: Lens.Lens' OnlineAbDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural))
onlineAbDefinition_treatmentWeights = Lens.lens (\OnlineAbDefinition' {treatmentWeights} -> treatmentWeights) (\s@OnlineAbDefinition' {} a -> s {treatmentWeights = a} :: OnlineAbDefinition) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON OnlineAbDefinition where
  parseJSON =
    Data.withObject
      "OnlineAbDefinition"
      ( \x ->
          OnlineAbDefinition'
            Prelude.<$> (x Data..:? "controlTreatmentName")
            Prelude.<*> ( x
                            Data..:? "treatmentWeights"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OnlineAbDefinition where
  hashWithSalt _salt OnlineAbDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` controlTreatmentName
      `Prelude.hashWithSalt` treatmentWeights

instance Prelude.NFData OnlineAbDefinition where
  rnf OnlineAbDefinition' {..} =
    Prelude.rnf controlTreatmentName
      `Prelude.seq` Prelude.rnf treatmentWeights
