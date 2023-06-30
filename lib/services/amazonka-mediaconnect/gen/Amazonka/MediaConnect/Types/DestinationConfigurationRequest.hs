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
-- Module      : Amazonka.MediaConnect.Types.DestinationConfigurationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.DestinationConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.InterfaceRequest
import qualified Amazonka.Prelude as Prelude

-- | The transport parameters that you want to associate with an outbound
-- media stream.
--
-- /See:/ 'newDestinationConfigurationRequest' smart constructor.
data DestinationConfigurationRequest = DestinationConfigurationRequest'
  { -- | The IP address where you want MediaConnect to send contents of the media
    -- stream.
    destinationIp :: Prelude.Text,
    -- | The port that you want MediaConnect to use when it distributes the media
    -- stream to the output.
    destinationPort :: Prelude.Int,
    -- | The VPC interface that you want to use for the media stream associated
    -- with the output.
    interface :: InterfaceRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationIp', 'destinationConfigurationRequest_destinationIp' - The IP address where you want MediaConnect to send contents of the media
-- stream.
--
-- 'destinationPort', 'destinationConfigurationRequest_destinationPort' - The port that you want MediaConnect to use when it distributes the media
-- stream to the output.
--
-- 'interface', 'destinationConfigurationRequest_interface' - The VPC interface that you want to use for the media stream associated
-- with the output.
newDestinationConfigurationRequest ::
  -- | 'destinationIp'
  Prelude.Text ->
  -- | 'destinationPort'
  Prelude.Int ->
  -- | 'interface'
  InterfaceRequest ->
  DestinationConfigurationRequest
newDestinationConfigurationRequest
  pDestinationIp_
  pDestinationPort_
  pInterface_ =
    DestinationConfigurationRequest'
      { destinationIp =
          pDestinationIp_,
        destinationPort = pDestinationPort_,
        interface = pInterface_
      }

-- | The IP address where you want MediaConnect to send contents of the media
-- stream.
destinationConfigurationRequest_destinationIp :: Lens.Lens' DestinationConfigurationRequest Prelude.Text
destinationConfigurationRequest_destinationIp = Lens.lens (\DestinationConfigurationRequest' {destinationIp} -> destinationIp) (\s@DestinationConfigurationRequest' {} a -> s {destinationIp = a} :: DestinationConfigurationRequest)

-- | The port that you want MediaConnect to use when it distributes the media
-- stream to the output.
destinationConfigurationRequest_destinationPort :: Lens.Lens' DestinationConfigurationRequest Prelude.Int
destinationConfigurationRequest_destinationPort = Lens.lens (\DestinationConfigurationRequest' {destinationPort} -> destinationPort) (\s@DestinationConfigurationRequest' {} a -> s {destinationPort = a} :: DestinationConfigurationRequest)

-- | The VPC interface that you want to use for the media stream associated
-- with the output.
destinationConfigurationRequest_interface :: Lens.Lens' DestinationConfigurationRequest InterfaceRequest
destinationConfigurationRequest_interface = Lens.lens (\DestinationConfigurationRequest' {interface} -> interface) (\s@DestinationConfigurationRequest' {} a -> s {interface = a} :: DestinationConfigurationRequest)

instance
  Prelude.Hashable
    DestinationConfigurationRequest
  where
  hashWithSalt
    _salt
    DestinationConfigurationRequest' {..} =
      _salt
        `Prelude.hashWithSalt` destinationIp
        `Prelude.hashWithSalt` destinationPort
        `Prelude.hashWithSalt` interface

instance
  Prelude.NFData
    DestinationConfigurationRequest
  where
  rnf DestinationConfigurationRequest' {..} =
    Prelude.rnf destinationIp
      `Prelude.seq` Prelude.rnf destinationPort
      `Prelude.seq` Prelude.rnf interface

instance Data.ToJSON DestinationConfigurationRequest where
  toJSON DestinationConfigurationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("destinationIp" Data..= destinationIp),
            Prelude.Just
              ("destinationPort" Data..= destinationPort),
            Prelude.Just ("interface" Data..= interface)
          ]
      )
