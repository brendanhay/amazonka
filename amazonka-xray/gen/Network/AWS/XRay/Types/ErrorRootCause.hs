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
-- Module      : Network.AWS.XRay.Types.ErrorRootCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorRootCause where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.ErrorRootCauseService

-- | The root cause of a trace summary error.
--
-- /See:/ 'newErrorRootCause' smart constructor.
data ErrorRootCause = ErrorRootCause'
  { -- | A list of services corresponding to an error. A service identifies a
    -- segment and it contains a name, account ID, type, and inferred flag.
    services :: Core.Maybe [ErrorRootCauseService],
    -- | A flag that denotes that the root cause impacts the trace client.
    clientImpacting :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ErrorRootCause' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'services', 'errorRootCause_services' - A list of services corresponding to an error. A service identifies a
-- segment and it contains a name, account ID, type, and inferred flag.
--
-- 'clientImpacting', 'errorRootCause_clientImpacting' - A flag that denotes that the root cause impacts the trace client.
newErrorRootCause ::
  ErrorRootCause
newErrorRootCause =
  ErrorRootCause'
    { services = Core.Nothing,
      clientImpacting = Core.Nothing
    }

-- | A list of services corresponding to an error. A service identifies a
-- segment and it contains a name, account ID, type, and inferred flag.
errorRootCause_services :: Lens.Lens' ErrorRootCause (Core.Maybe [ErrorRootCauseService])
errorRootCause_services = Lens.lens (\ErrorRootCause' {services} -> services) (\s@ErrorRootCause' {} a -> s {services = a} :: ErrorRootCause) Core.. Lens.mapping Lens._Coerce

-- | A flag that denotes that the root cause impacts the trace client.
errorRootCause_clientImpacting :: Lens.Lens' ErrorRootCause (Core.Maybe Core.Bool)
errorRootCause_clientImpacting = Lens.lens (\ErrorRootCause' {clientImpacting} -> clientImpacting) (\s@ErrorRootCause' {} a -> s {clientImpacting = a} :: ErrorRootCause)

instance Core.FromJSON ErrorRootCause where
  parseJSON =
    Core.withObject
      "ErrorRootCause"
      ( \x ->
          ErrorRootCause'
            Core.<$> (x Core..:? "Services" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ClientImpacting")
      )

instance Core.Hashable ErrorRootCause

instance Core.NFData ErrorRootCause
