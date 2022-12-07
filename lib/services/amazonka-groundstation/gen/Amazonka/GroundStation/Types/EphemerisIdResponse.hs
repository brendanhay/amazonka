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
-- Module      : Amazonka.GroundStation.Types.EphemerisIdResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisIdResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newEphemerisIdResponse' smart constructor.
data EphemerisIdResponse = EphemerisIdResponse'
  { -- | The AWS Ground Station ephemeris ID.
    ephemerisId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EphemerisIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ephemerisId', 'ephemerisIdResponse_ephemerisId' - The AWS Ground Station ephemeris ID.
newEphemerisIdResponse ::
  EphemerisIdResponse
newEphemerisIdResponse =
  EphemerisIdResponse' {ephemerisId = Prelude.Nothing}

-- | The AWS Ground Station ephemeris ID.
ephemerisIdResponse_ephemerisId :: Lens.Lens' EphemerisIdResponse (Prelude.Maybe Prelude.Text)
ephemerisIdResponse_ephemerisId = Lens.lens (\EphemerisIdResponse' {ephemerisId} -> ephemerisId) (\s@EphemerisIdResponse' {} a -> s {ephemerisId = a} :: EphemerisIdResponse)

instance Data.FromJSON EphemerisIdResponse where
  parseJSON =
    Data.withObject
      "EphemerisIdResponse"
      ( \x ->
          EphemerisIdResponse'
            Prelude.<$> (x Data..:? "ephemerisId")
      )

instance Prelude.Hashable EphemerisIdResponse where
  hashWithSalt _salt EphemerisIdResponse' {..} =
    _salt `Prelude.hashWithSalt` ephemerisId

instance Prelude.NFData EphemerisIdResponse where
  rnf EphemerisIdResponse' {..} =
    Prelude.rnf ephemerisId
