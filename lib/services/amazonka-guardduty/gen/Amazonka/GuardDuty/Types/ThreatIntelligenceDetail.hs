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
-- Module      : Amazonka.GuardDuty.Types.ThreatIntelligenceDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ThreatIntelligenceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An instance of a threat intelligence detail that constitutes evidence
-- for the finding.
--
-- /See:/ 'newThreatIntelligenceDetail' smart constructor.
data ThreatIntelligenceDetail = ThreatIntelligenceDetail'
  { -- | The name of the threat intelligence list that triggered the finding.
    threatListName :: Prelude.Maybe Prelude.Text,
    -- | A list of names of the threats in the threat intelligence list that
    -- triggered the finding.
    threatNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThreatIntelligenceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threatListName', 'threatIntelligenceDetail_threatListName' - The name of the threat intelligence list that triggered the finding.
--
-- 'threatNames', 'threatIntelligenceDetail_threatNames' - A list of names of the threats in the threat intelligence list that
-- triggered the finding.
newThreatIntelligenceDetail ::
  ThreatIntelligenceDetail
newThreatIntelligenceDetail =
  ThreatIntelligenceDetail'
    { threatListName =
        Prelude.Nothing,
      threatNames = Prelude.Nothing
    }

-- | The name of the threat intelligence list that triggered the finding.
threatIntelligenceDetail_threatListName :: Lens.Lens' ThreatIntelligenceDetail (Prelude.Maybe Prelude.Text)
threatIntelligenceDetail_threatListName = Lens.lens (\ThreatIntelligenceDetail' {threatListName} -> threatListName) (\s@ThreatIntelligenceDetail' {} a -> s {threatListName = a} :: ThreatIntelligenceDetail)

-- | A list of names of the threats in the threat intelligence list that
-- triggered the finding.
threatIntelligenceDetail_threatNames :: Lens.Lens' ThreatIntelligenceDetail (Prelude.Maybe [Prelude.Text])
threatIntelligenceDetail_threatNames = Lens.lens (\ThreatIntelligenceDetail' {threatNames} -> threatNames) (\s@ThreatIntelligenceDetail' {} a -> s {threatNames = a} :: ThreatIntelligenceDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ThreatIntelligenceDetail where
  parseJSON =
    Data.withObject
      "ThreatIntelligenceDetail"
      ( \x ->
          ThreatIntelligenceDetail'
            Prelude.<$> (x Data..:? "threatListName")
            Prelude.<*> (x Data..:? "threatNames" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ThreatIntelligenceDetail where
  hashWithSalt _salt ThreatIntelligenceDetail' {..} =
    _salt
      `Prelude.hashWithSalt` threatListName
      `Prelude.hashWithSalt` threatNames

instance Prelude.NFData ThreatIntelligenceDetail where
  rnf ThreatIntelligenceDetail' {..} =
    Prelude.rnf threatListName
      `Prelude.seq` Prelude.rnf threatNames
