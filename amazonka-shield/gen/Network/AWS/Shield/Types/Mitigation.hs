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
-- Module      : Network.AWS.Shield.Types.Mitigation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Mitigation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The mitigation applied to a DDoS attack.
--
-- /See:/ 'newMitigation' smart constructor.
data Mitigation = Mitigation'
  { -- | The name of the mitigation taken for this attack.
    mitigationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Mitigation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mitigationName', 'mitigation_mitigationName' - The name of the mitigation taken for this attack.
newMitigation ::
  Mitigation
newMitigation =
  Mitigation' {mitigationName = Prelude.Nothing}

-- | The name of the mitigation taken for this attack.
mitigation_mitigationName :: Lens.Lens' Mitigation (Prelude.Maybe Prelude.Text)
mitigation_mitigationName = Lens.lens (\Mitigation' {mitigationName} -> mitigationName) (\s@Mitigation' {} a -> s {mitigationName = a} :: Mitigation)

instance Prelude.FromJSON Mitigation where
  parseJSON =
    Prelude.withObject
      "Mitigation"
      ( \x ->
          Mitigation'
            Prelude.<$> (x Prelude..:? "MitigationName")
      )

instance Prelude.Hashable Mitigation

instance Prelude.NFData Mitigation
