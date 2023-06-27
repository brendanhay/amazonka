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
-- Module      : Amazonka.IoTWireless.Types.GlobalIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.GlobalIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Global identity information.
--
-- /See:/ 'newGlobalIdentity' smart constructor.
data GlobalIdentity = GlobalIdentity'
  { -- | Location area code of the global identity.
    lac :: Prelude.Natural,
    -- | GERAN (GSM EDGE Radio Access Network) cell global identifier.
    geranCid :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lac', 'globalIdentity_lac' - Location area code of the global identity.
--
-- 'geranCid', 'globalIdentity_geranCid' - GERAN (GSM EDGE Radio Access Network) cell global identifier.
newGlobalIdentity ::
  -- | 'lac'
  Prelude.Natural ->
  -- | 'geranCid'
  Prelude.Natural ->
  GlobalIdentity
newGlobalIdentity pLac_ pGeranCid_ =
  GlobalIdentity' {lac = pLac_, geranCid = pGeranCid_}

-- | Location area code of the global identity.
globalIdentity_lac :: Lens.Lens' GlobalIdentity Prelude.Natural
globalIdentity_lac = Lens.lens (\GlobalIdentity' {lac} -> lac) (\s@GlobalIdentity' {} a -> s {lac = a} :: GlobalIdentity)

-- | GERAN (GSM EDGE Radio Access Network) cell global identifier.
globalIdentity_geranCid :: Lens.Lens' GlobalIdentity Prelude.Natural
globalIdentity_geranCid = Lens.lens (\GlobalIdentity' {geranCid} -> geranCid) (\s@GlobalIdentity' {} a -> s {geranCid = a} :: GlobalIdentity)

instance Prelude.Hashable GlobalIdentity where
  hashWithSalt _salt GlobalIdentity' {..} =
    _salt
      `Prelude.hashWithSalt` lac
      `Prelude.hashWithSalt` geranCid

instance Prelude.NFData GlobalIdentity where
  rnf GlobalIdentity' {..} =
    Prelude.rnf lac `Prelude.seq` Prelude.rnf geranCid

instance Data.ToJSON GlobalIdentity where
  toJSON GlobalIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Lac" Data..= lac),
            Prelude.Just ("GeranCid" Data..= geranCid)
          ]
      )
