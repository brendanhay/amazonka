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
-- Module      : Amazonka.MediaConnect.Types.InputConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.InputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types.Interface
import qualified Amazonka.Prelude as Prelude

-- | The transport parameters that are associated with an incoming media
-- stream.
--
-- /See:/ 'newInputConfiguration' smart constructor.
data InputConfiguration = InputConfiguration'
  { -- | The port that the flow listens on for an incoming media stream.
    inputPort :: Prelude.Int,
    -- | The IP address that the flow listens on for incoming content for a media
    -- stream.
    inputIp :: Prelude.Text,
    -- | The VPC interface where the media stream comes in from.
    interface :: Interface
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputPort', 'inputConfiguration_inputPort' - The port that the flow listens on for an incoming media stream.
--
-- 'inputIp', 'inputConfiguration_inputIp' - The IP address that the flow listens on for incoming content for a media
-- stream.
--
-- 'interface', 'inputConfiguration_interface' - The VPC interface where the media stream comes in from.
newInputConfiguration ::
  -- | 'inputPort'
  Prelude.Int ->
  -- | 'inputIp'
  Prelude.Text ->
  -- | 'interface'
  Interface ->
  InputConfiguration
newInputConfiguration
  pInputPort_
  pInputIp_
  pInterface_ =
    InputConfiguration'
      { inputPort = pInputPort_,
        inputIp = pInputIp_,
        interface = pInterface_
      }

-- | The port that the flow listens on for an incoming media stream.
inputConfiguration_inputPort :: Lens.Lens' InputConfiguration Prelude.Int
inputConfiguration_inputPort = Lens.lens (\InputConfiguration' {inputPort} -> inputPort) (\s@InputConfiguration' {} a -> s {inputPort = a} :: InputConfiguration)

-- | The IP address that the flow listens on for incoming content for a media
-- stream.
inputConfiguration_inputIp :: Lens.Lens' InputConfiguration Prelude.Text
inputConfiguration_inputIp = Lens.lens (\InputConfiguration' {inputIp} -> inputIp) (\s@InputConfiguration' {} a -> s {inputIp = a} :: InputConfiguration)

-- | The VPC interface where the media stream comes in from.
inputConfiguration_interface :: Lens.Lens' InputConfiguration Interface
inputConfiguration_interface = Lens.lens (\InputConfiguration' {interface} -> interface) (\s@InputConfiguration' {} a -> s {interface = a} :: InputConfiguration)

instance Core.FromJSON InputConfiguration where
  parseJSON =
    Core.withObject
      "InputConfiguration"
      ( \x ->
          InputConfiguration'
            Prelude.<$> (x Core..: "inputPort")
            Prelude.<*> (x Core..: "inputIp")
            Prelude.<*> (x Core..: "interface")
      )

instance Prelude.Hashable InputConfiguration where
  hashWithSalt _salt InputConfiguration' {..} =
    _salt `Prelude.hashWithSalt` inputPort
      `Prelude.hashWithSalt` inputIp
      `Prelude.hashWithSalt` interface

instance Prelude.NFData InputConfiguration where
  rnf InputConfiguration' {..} =
    Prelude.rnf inputPort
      `Prelude.seq` Prelude.rnf inputIp
      `Prelude.seq` Prelude.rnf interface
