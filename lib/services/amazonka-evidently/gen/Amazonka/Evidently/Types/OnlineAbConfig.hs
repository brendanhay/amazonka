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
-- Module      : Amazonka.Evidently.Types.OnlineAbConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.OnlineAbConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration of which variation to use as
-- the \"control\" version. The \"control\" version is used for comparison
-- with other variations. This structure also specifies how much experiment
-- traffic is allocated to each variation.
--
-- /See:/ 'newOnlineAbConfig' smart constructor.
data OnlineAbConfig = OnlineAbConfig'
  { -- | The name of the variation that is to be the default variation that the
    -- other variations are compared to.
    controlTreatmentName :: Prelude.Maybe Prelude.Text,
    -- | A set of key-value pairs. The keys are variation names, and the values
    -- are the portion of experiment traffic to be assigned to that variation.
    -- Specify the traffic portion in thousandths of a percent, so 20,000 for a
    -- variation would allocate 20% of the experiment traffic to that
    -- variation.
    treatmentWeights :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnlineAbConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlTreatmentName', 'onlineAbConfig_controlTreatmentName' - The name of the variation that is to be the default variation that the
-- other variations are compared to.
--
-- 'treatmentWeights', 'onlineAbConfig_treatmentWeights' - A set of key-value pairs. The keys are variation names, and the values
-- are the portion of experiment traffic to be assigned to that variation.
-- Specify the traffic portion in thousandths of a percent, so 20,000 for a
-- variation would allocate 20% of the experiment traffic to that
-- variation.
newOnlineAbConfig ::
  OnlineAbConfig
newOnlineAbConfig =
  OnlineAbConfig'
    { controlTreatmentName =
        Prelude.Nothing,
      treatmentWeights = Prelude.Nothing
    }

-- | The name of the variation that is to be the default variation that the
-- other variations are compared to.
onlineAbConfig_controlTreatmentName :: Lens.Lens' OnlineAbConfig (Prelude.Maybe Prelude.Text)
onlineAbConfig_controlTreatmentName = Lens.lens (\OnlineAbConfig' {controlTreatmentName} -> controlTreatmentName) (\s@OnlineAbConfig' {} a -> s {controlTreatmentName = a} :: OnlineAbConfig)

-- | A set of key-value pairs. The keys are variation names, and the values
-- are the portion of experiment traffic to be assigned to that variation.
-- Specify the traffic portion in thousandths of a percent, so 20,000 for a
-- variation would allocate 20% of the experiment traffic to that
-- variation.
onlineAbConfig_treatmentWeights :: Lens.Lens' OnlineAbConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Natural))
onlineAbConfig_treatmentWeights = Lens.lens (\OnlineAbConfig' {treatmentWeights} -> treatmentWeights) (\s@OnlineAbConfig' {} a -> s {treatmentWeights = a} :: OnlineAbConfig) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable OnlineAbConfig where
  hashWithSalt _salt OnlineAbConfig' {..} =
    _salt
      `Prelude.hashWithSalt` controlTreatmentName
      `Prelude.hashWithSalt` treatmentWeights

instance Prelude.NFData OnlineAbConfig where
  rnf OnlineAbConfig' {..} =
    Prelude.rnf controlTreatmentName
      `Prelude.seq` Prelude.rnf treatmentWeights

instance Data.ToJSON OnlineAbConfig where
  toJSON OnlineAbConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("controlTreatmentName" Data..=)
              Prelude.<$> controlTreatmentName,
            ("treatmentWeights" Data..=)
              Prelude.<$> treatmentWeights
          ]
      )
