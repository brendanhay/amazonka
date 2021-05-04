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
-- Module      : Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An instance of a threat intelligence detail that constitutes evidence
-- for the finding.
--
-- /See:/ 'newThreatIntelligenceDetail' smart constructor.
data ThreatIntelligenceDetail = ThreatIntelligenceDetail'
  { -- | A list of names of the threats in the threat intelligence list that
    -- triggered the finding.
    threatNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the threat intelligence list that triggered the finding.
    threatListName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ThreatIntelligenceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threatNames', 'threatIntelligenceDetail_threatNames' - A list of names of the threats in the threat intelligence list that
-- triggered the finding.
--
-- 'threatListName', 'threatIntelligenceDetail_threatListName' - The name of the threat intelligence list that triggered the finding.
newThreatIntelligenceDetail ::
  ThreatIntelligenceDetail
newThreatIntelligenceDetail =
  ThreatIntelligenceDetail'
    { threatNames =
        Prelude.Nothing,
      threatListName = Prelude.Nothing
    }

-- | A list of names of the threats in the threat intelligence list that
-- triggered the finding.
threatIntelligenceDetail_threatNames :: Lens.Lens' ThreatIntelligenceDetail (Prelude.Maybe [Prelude.Text])
threatIntelligenceDetail_threatNames = Lens.lens (\ThreatIntelligenceDetail' {threatNames} -> threatNames) (\s@ThreatIntelligenceDetail' {} a -> s {threatNames = a} :: ThreatIntelligenceDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the threat intelligence list that triggered the finding.
threatIntelligenceDetail_threatListName :: Lens.Lens' ThreatIntelligenceDetail (Prelude.Maybe Prelude.Text)
threatIntelligenceDetail_threatListName = Lens.lens (\ThreatIntelligenceDetail' {threatListName} -> threatListName) (\s@ThreatIntelligenceDetail' {} a -> s {threatListName = a} :: ThreatIntelligenceDetail)

instance Prelude.FromJSON ThreatIntelligenceDetail where
  parseJSON =
    Prelude.withObject
      "ThreatIntelligenceDetail"
      ( \x ->
          ThreatIntelligenceDetail'
            Prelude.<$> ( x Prelude..:? "threatNames"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "threatListName")
      )

instance Prelude.Hashable ThreatIntelligenceDetail

instance Prelude.NFData ThreatIntelligenceDetail
