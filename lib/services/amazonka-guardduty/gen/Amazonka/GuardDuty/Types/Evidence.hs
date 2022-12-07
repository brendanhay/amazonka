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
-- Module      : Amazonka.GuardDuty.Types.Evidence
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Evidence where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.ThreatIntelligenceDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the reason that the finding was generated.
--
-- /See:/ 'newEvidence' smart constructor.
data Evidence = Evidence'
  { -- | A list of threat intelligence details related to the evidence.
    threatIntelligenceDetails :: Prelude.Maybe [ThreatIntelligenceDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Evidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threatIntelligenceDetails', 'evidence_threatIntelligenceDetails' - A list of threat intelligence details related to the evidence.
newEvidence ::
  Evidence
newEvidence =
  Evidence'
    { threatIntelligenceDetails =
        Prelude.Nothing
    }

-- | A list of threat intelligence details related to the evidence.
evidence_threatIntelligenceDetails :: Lens.Lens' Evidence (Prelude.Maybe [ThreatIntelligenceDetail])
evidence_threatIntelligenceDetails = Lens.lens (\Evidence' {threatIntelligenceDetails} -> threatIntelligenceDetails) (\s@Evidence' {} a -> s {threatIntelligenceDetails = a} :: Evidence) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Evidence where
  parseJSON =
    Data.withObject
      "Evidence"
      ( \x ->
          Evidence'
            Prelude.<$> ( x Data..:? "threatIntelligenceDetails"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Evidence where
  hashWithSalt _salt Evidence' {..} =
    _salt
      `Prelude.hashWithSalt` threatIntelligenceDetails

instance Prelude.NFData Evidence where
  rnf Evidence' {..} =
    Prelude.rnf threatIntelligenceDetails
