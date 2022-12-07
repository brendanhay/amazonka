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
-- Module      : Amazonka.XRay.Types.FaultRootCause
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.FaultRootCause where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.FaultRootCauseService

-- | The root cause information for a trace summary fault.
--
-- /See:/ 'newFaultRootCause' smart constructor.
data FaultRootCause = FaultRootCause'
  { -- | A list of corresponding services. A service identifies a segment and it
    -- contains a name, account ID, type, and inferred flag.
    services :: Prelude.Maybe [FaultRootCauseService],
    -- | A flag that denotes that the root cause impacts the trace client.
    clientImpacting :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaultRootCause' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'services', 'faultRootCause_services' - A list of corresponding services. A service identifies a segment and it
-- contains a name, account ID, type, and inferred flag.
--
-- 'clientImpacting', 'faultRootCause_clientImpacting' - A flag that denotes that the root cause impacts the trace client.
newFaultRootCause ::
  FaultRootCause
newFaultRootCause =
  FaultRootCause'
    { services = Prelude.Nothing,
      clientImpacting = Prelude.Nothing
    }

-- | A list of corresponding services. A service identifies a segment and it
-- contains a name, account ID, type, and inferred flag.
faultRootCause_services :: Lens.Lens' FaultRootCause (Prelude.Maybe [FaultRootCauseService])
faultRootCause_services = Lens.lens (\FaultRootCause' {services} -> services) (\s@FaultRootCause' {} a -> s {services = a} :: FaultRootCause) Prelude.. Lens.mapping Lens.coerced

-- | A flag that denotes that the root cause impacts the trace client.
faultRootCause_clientImpacting :: Lens.Lens' FaultRootCause (Prelude.Maybe Prelude.Bool)
faultRootCause_clientImpacting = Lens.lens (\FaultRootCause' {clientImpacting} -> clientImpacting) (\s@FaultRootCause' {} a -> s {clientImpacting = a} :: FaultRootCause)

instance Data.FromJSON FaultRootCause where
  parseJSON =
    Data.withObject
      "FaultRootCause"
      ( \x ->
          FaultRootCause'
            Prelude.<$> (x Data..:? "Services" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ClientImpacting")
      )

instance Prelude.Hashable FaultRootCause where
  hashWithSalt _salt FaultRootCause' {..} =
    _salt `Prelude.hashWithSalt` services
      `Prelude.hashWithSalt` clientImpacting

instance Prelude.NFData FaultRootCause where
  rnf FaultRootCause' {..} =
    Prelude.rnf services
      `Prelude.seq` Prelude.rnf clientImpacting
