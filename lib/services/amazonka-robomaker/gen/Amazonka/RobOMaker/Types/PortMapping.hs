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
-- Module      : Amazonka.RobOMaker.Types.PortMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.PortMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a port mapping.
--
-- /See:/ 'newPortMapping' smart constructor.
data PortMapping = PortMapping'
  { -- | A Boolean indicating whether to enable this port mapping on public IP.
    enableOnPublicIp :: Prelude.Maybe Prelude.Bool,
    -- | The port number on the simulation job instance to use as a remote
    -- connection point.
    jobPort :: Prelude.Natural,
    -- | The port number on the application.
    applicationPort :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableOnPublicIp', 'portMapping_enableOnPublicIp' - A Boolean indicating whether to enable this port mapping on public IP.
--
-- 'jobPort', 'portMapping_jobPort' - The port number on the simulation job instance to use as a remote
-- connection point.
--
-- 'applicationPort', 'portMapping_applicationPort' - The port number on the application.
newPortMapping ::
  -- | 'jobPort'
  Prelude.Natural ->
  -- | 'applicationPort'
  Prelude.Natural ->
  PortMapping
newPortMapping pJobPort_ pApplicationPort_ =
  PortMapping'
    { enableOnPublicIp = Prelude.Nothing,
      jobPort = pJobPort_,
      applicationPort = pApplicationPort_
    }

-- | A Boolean indicating whether to enable this port mapping on public IP.
portMapping_enableOnPublicIp :: Lens.Lens' PortMapping (Prelude.Maybe Prelude.Bool)
portMapping_enableOnPublicIp = Lens.lens (\PortMapping' {enableOnPublicIp} -> enableOnPublicIp) (\s@PortMapping' {} a -> s {enableOnPublicIp = a} :: PortMapping)

-- | The port number on the simulation job instance to use as a remote
-- connection point.
portMapping_jobPort :: Lens.Lens' PortMapping Prelude.Natural
portMapping_jobPort = Lens.lens (\PortMapping' {jobPort} -> jobPort) (\s@PortMapping' {} a -> s {jobPort = a} :: PortMapping)

-- | The port number on the application.
portMapping_applicationPort :: Lens.Lens' PortMapping Prelude.Natural
portMapping_applicationPort = Lens.lens (\PortMapping' {applicationPort} -> applicationPort) (\s@PortMapping' {} a -> s {applicationPort = a} :: PortMapping)

instance Data.FromJSON PortMapping where
  parseJSON =
    Data.withObject
      "PortMapping"
      ( \x ->
          PortMapping'
            Prelude.<$> (x Data..:? "enableOnPublicIp")
            Prelude.<*> (x Data..: "jobPort")
            Prelude.<*> (x Data..: "applicationPort")
      )

instance Prelude.Hashable PortMapping where
  hashWithSalt _salt PortMapping' {..} =
    _salt
      `Prelude.hashWithSalt` enableOnPublicIp
      `Prelude.hashWithSalt` jobPort
      `Prelude.hashWithSalt` applicationPort

instance Prelude.NFData PortMapping where
  rnf PortMapping' {..} =
    Prelude.rnf enableOnPublicIp `Prelude.seq`
      Prelude.rnf jobPort `Prelude.seq`
        Prelude.rnf applicationPort

instance Data.ToJSON PortMapping where
  toJSON PortMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("enableOnPublicIp" Data..=)
              Prelude.<$> enableOnPublicIp,
            Prelude.Just ("jobPort" Data..= jobPort),
            Prelude.Just
              ("applicationPort" Data..= applicationPort)
          ]
      )
