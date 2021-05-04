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
-- Module      : Network.AWS.XRay.Types.ResponseTimeRootCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ResponseTimeRootCause where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.ResponseTimeRootCauseService

-- | The root cause information for a response time warning.
--
-- /See:/ 'newResponseTimeRootCause' smart constructor.
data ResponseTimeRootCause = ResponseTimeRootCause'
  { -- | A list of corresponding services. A service identifies a segment and
    -- contains a name, account ID, type, and inferred flag.
    services :: Prelude.Maybe [ResponseTimeRootCauseService],
    -- | A flag that denotes that the root cause impacts the trace client.
    clientImpacting :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResponseTimeRootCause' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'services', 'responseTimeRootCause_services' - A list of corresponding services. A service identifies a segment and
-- contains a name, account ID, type, and inferred flag.
--
-- 'clientImpacting', 'responseTimeRootCause_clientImpacting' - A flag that denotes that the root cause impacts the trace client.
newResponseTimeRootCause ::
  ResponseTimeRootCause
newResponseTimeRootCause =
  ResponseTimeRootCause'
    { services = Prelude.Nothing,
      clientImpacting = Prelude.Nothing
    }

-- | A list of corresponding services. A service identifies a segment and
-- contains a name, account ID, type, and inferred flag.
responseTimeRootCause_services :: Lens.Lens' ResponseTimeRootCause (Prelude.Maybe [ResponseTimeRootCauseService])
responseTimeRootCause_services = Lens.lens (\ResponseTimeRootCause' {services} -> services) (\s@ResponseTimeRootCause' {} a -> s {services = a} :: ResponseTimeRootCause) Prelude.. Lens.mapping Prelude._Coerce

-- | A flag that denotes that the root cause impacts the trace client.
responseTimeRootCause_clientImpacting :: Lens.Lens' ResponseTimeRootCause (Prelude.Maybe Prelude.Bool)
responseTimeRootCause_clientImpacting = Lens.lens (\ResponseTimeRootCause' {clientImpacting} -> clientImpacting) (\s@ResponseTimeRootCause' {} a -> s {clientImpacting = a} :: ResponseTimeRootCause)

instance Prelude.FromJSON ResponseTimeRootCause where
  parseJSON =
    Prelude.withObject
      "ResponseTimeRootCause"
      ( \x ->
          ResponseTimeRootCause'
            Prelude.<$> (x Prelude..:? "Services" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "ClientImpacting")
      )

instance Prelude.Hashable ResponseTimeRootCause

instance Prelude.NFData ResponseTimeRootCause
