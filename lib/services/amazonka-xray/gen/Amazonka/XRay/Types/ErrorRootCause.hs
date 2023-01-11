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
-- Module      : Amazonka.XRay.Types.ErrorRootCause
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ErrorRootCause where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.ErrorRootCauseService

-- | The root cause of a trace summary error.
--
-- /See:/ 'newErrorRootCause' smart constructor.
data ErrorRootCause = ErrorRootCause'
  { -- | A flag that denotes that the root cause impacts the trace client.
    clientImpacting :: Prelude.Maybe Prelude.Bool,
    -- | A list of services corresponding to an error. A service identifies a
    -- segment and it contains a name, account ID, type, and inferred flag.
    services :: Prelude.Maybe [ErrorRootCauseService]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorRootCause' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientImpacting', 'errorRootCause_clientImpacting' - A flag that denotes that the root cause impacts the trace client.
--
-- 'services', 'errorRootCause_services' - A list of services corresponding to an error. A service identifies a
-- segment and it contains a name, account ID, type, and inferred flag.
newErrorRootCause ::
  ErrorRootCause
newErrorRootCause =
  ErrorRootCause'
    { clientImpacting = Prelude.Nothing,
      services = Prelude.Nothing
    }

-- | A flag that denotes that the root cause impacts the trace client.
errorRootCause_clientImpacting :: Lens.Lens' ErrorRootCause (Prelude.Maybe Prelude.Bool)
errorRootCause_clientImpacting = Lens.lens (\ErrorRootCause' {clientImpacting} -> clientImpacting) (\s@ErrorRootCause' {} a -> s {clientImpacting = a} :: ErrorRootCause)

-- | A list of services corresponding to an error. A service identifies a
-- segment and it contains a name, account ID, type, and inferred flag.
errorRootCause_services :: Lens.Lens' ErrorRootCause (Prelude.Maybe [ErrorRootCauseService])
errorRootCause_services = Lens.lens (\ErrorRootCause' {services} -> services) (\s@ErrorRootCause' {} a -> s {services = a} :: ErrorRootCause) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ErrorRootCause where
  parseJSON =
    Data.withObject
      "ErrorRootCause"
      ( \x ->
          ErrorRootCause'
            Prelude.<$> (x Data..:? "ClientImpacting")
            Prelude.<*> (x Data..:? "Services" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ErrorRootCause where
  hashWithSalt _salt ErrorRootCause' {..} =
    _salt `Prelude.hashWithSalt` clientImpacting
      `Prelude.hashWithSalt` services

instance Prelude.NFData ErrorRootCause where
  rnf ErrorRootCause' {..} =
    Prelude.rnf clientImpacting
      `Prelude.seq` Prelude.rnf services
