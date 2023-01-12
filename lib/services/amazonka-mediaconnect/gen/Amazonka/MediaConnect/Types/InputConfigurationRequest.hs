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
-- Module      : Amazonka.MediaConnect.Types.InputConfigurationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.InputConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.InterfaceRequest
import qualified Amazonka.Prelude as Prelude

-- | The transport parameters that you want to associate with an incoming
-- media stream.
--
-- /See:/ 'newInputConfigurationRequest' smart constructor.
data InputConfigurationRequest = InputConfigurationRequest'
  { -- | The port that you want the flow to listen on for an incoming media
    -- stream.
    inputPort :: Prelude.Int,
    -- | The VPC interface that you want to use for the incoming media stream.
    interface :: InterfaceRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputPort', 'inputConfigurationRequest_inputPort' - The port that you want the flow to listen on for an incoming media
-- stream.
--
-- 'interface', 'inputConfigurationRequest_interface' - The VPC interface that you want to use for the incoming media stream.
newInputConfigurationRequest ::
  -- | 'inputPort'
  Prelude.Int ->
  -- | 'interface'
  InterfaceRequest ->
  InputConfigurationRequest
newInputConfigurationRequest pInputPort_ pInterface_ =
  InputConfigurationRequest'
    { inputPort = pInputPort_,
      interface = pInterface_
    }

-- | The port that you want the flow to listen on for an incoming media
-- stream.
inputConfigurationRequest_inputPort :: Lens.Lens' InputConfigurationRequest Prelude.Int
inputConfigurationRequest_inputPort = Lens.lens (\InputConfigurationRequest' {inputPort} -> inputPort) (\s@InputConfigurationRequest' {} a -> s {inputPort = a} :: InputConfigurationRequest)

-- | The VPC interface that you want to use for the incoming media stream.
inputConfigurationRequest_interface :: Lens.Lens' InputConfigurationRequest InterfaceRequest
inputConfigurationRequest_interface = Lens.lens (\InputConfigurationRequest' {interface} -> interface) (\s@InputConfigurationRequest' {} a -> s {interface = a} :: InputConfigurationRequest)

instance Prelude.Hashable InputConfigurationRequest where
  hashWithSalt _salt InputConfigurationRequest' {..} =
    _salt `Prelude.hashWithSalt` inputPort
      `Prelude.hashWithSalt` interface

instance Prelude.NFData InputConfigurationRequest where
  rnf InputConfigurationRequest' {..} =
    Prelude.rnf inputPort
      `Prelude.seq` Prelude.rnf interface

instance Data.ToJSON InputConfigurationRequest where
  toJSON InputConfigurationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("inputPort" Data..= inputPort),
            Prelude.Just ("interface" Data..= interface)
          ]
      )
