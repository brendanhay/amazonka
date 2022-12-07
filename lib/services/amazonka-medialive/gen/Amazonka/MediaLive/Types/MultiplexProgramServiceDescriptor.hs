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
-- Module      : Amazonka.MediaLive.Types.MultiplexProgramServiceDescriptor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexProgramServiceDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Transport stream service descriptor configuration for the Multiplex
-- program.
--
-- /See:/ 'newMultiplexProgramServiceDescriptor' smart constructor.
data MultiplexProgramServiceDescriptor = MultiplexProgramServiceDescriptor'
  { -- | Name of the provider.
    providerName :: Prelude.Text,
    -- | Name of the service.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexProgramServiceDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'providerName', 'multiplexProgramServiceDescriptor_providerName' - Name of the provider.
--
-- 'serviceName', 'multiplexProgramServiceDescriptor_serviceName' - Name of the service.
newMultiplexProgramServiceDescriptor ::
  -- | 'providerName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  MultiplexProgramServiceDescriptor
newMultiplexProgramServiceDescriptor
  pProviderName_
  pServiceName_ =
    MultiplexProgramServiceDescriptor'
      { providerName =
          pProviderName_,
        serviceName = pServiceName_
      }

-- | Name of the provider.
multiplexProgramServiceDescriptor_providerName :: Lens.Lens' MultiplexProgramServiceDescriptor Prelude.Text
multiplexProgramServiceDescriptor_providerName = Lens.lens (\MultiplexProgramServiceDescriptor' {providerName} -> providerName) (\s@MultiplexProgramServiceDescriptor' {} a -> s {providerName = a} :: MultiplexProgramServiceDescriptor)

-- | Name of the service.
multiplexProgramServiceDescriptor_serviceName :: Lens.Lens' MultiplexProgramServiceDescriptor Prelude.Text
multiplexProgramServiceDescriptor_serviceName = Lens.lens (\MultiplexProgramServiceDescriptor' {serviceName} -> serviceName) (\s@MultiplexProgramServiceDescriptor' {} a -> s {serviceName = a} :: MultiplexProgramServiceDescriptor)

instance
  Data.FromJSON
    MultiplexProgramServiceDescriptor
  where
  parseJSON =
    Data.withObject
      "MultiplexProgramServiceDescriptor"
      ( \x ->
          MultiplexProgramServiceDescriptor'
            Prelude.<$> (x Data..: "providerName")
            Prelude.<*> (x Data..: "serviceName")
      )

instance
  Prelude.Hashable
    MultiplexProgramServiceDescriptor
  where
  hashWithSalt
    _salt
    MultiplexProgramServiceDescriptor' {..} =
      _salt `Prelude.hashWithSalt` providerName
        `Prelude.hashWithSalt` serviceName

instance
  Prelude.NFData
    MultiplexProgramServiceDescriptor
  where
  rnf MultiplexProgramServiceDescriptor' {..} =
    Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf serviceName

instance
  Data.ToJSON
    MultiplexProgramServiceDescriptor
  where
  toJSON MultiplexProgramServiceDescriptor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("providerName" Data..= providerName),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )
