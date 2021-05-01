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
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexProgramServiceDescriptor where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.FromJSON
    MultiplexProgramServiceDescriptor
  where
  parseJSON =
    Prelude.withObject
      "MultiplexProgramServiceDescriptor"
      ( \x ->
          MultiplexProgramServiceDescriptor'
            Prelude.<$> (x Prelude..: "providerName")
            Prelude.<*> (x Prelude..: "serviceName")
      )

instance
  Prelude.Hashable
    MultiplexProgramServiceDescriptor

instance
  Prelude.NFData
    MultiplexProgramServiceDescriptor

instance
  Prelude.ToJSON
    MultiplexProgramServiceDescriptor
  where
  toJSON MultiplexProgramServiceDescriptor' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("providerName" Prelude..= providerName),
            Prelude.Just ("serviceName" Prelude..= serviceName)
          ]
      )
