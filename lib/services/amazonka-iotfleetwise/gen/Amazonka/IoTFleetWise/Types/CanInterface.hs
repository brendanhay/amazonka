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
-- Module      : Amazonka.IoTFleetWise.Types.CanInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CanInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A single controller area network (CAN) device interface.
--
-- /See:/ 'newCanInterface' smart constructor.
data CanInterface = CanInterface'
  { -- | The name of the communication protocol for the interface.
    protocolName :: Prelude.Maybe Prelude.Text,
    -- | The version of the communication protocol for the interface.
    protocolVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the interface.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocolName', 'canInterface_protocolName' - The name of the communication protocol for the interface.
--
-- 'protocolVersion', 'canInterface_protocolVersion' - The version of the communication protocol for the interface.
--
-- 'name', 'canInterface_name' - The unique name of the interface.
newCanInterface ::
  -- | 'name'
  Prelude.Text ->
  CanInterface
newCanInterface pName_ =
  CanInterface'
    { protocolName = Prelude.Nothing,
      protocolVersion = Prelude.Nothing,
      name = pName_
    }

-- | The name of the communication protocol for the interface.
canInterface_protocolName :: Lens.Lens' CanInterface (Prelude.Maybe Prelude.Text)
canInterface_protocolName = Lens.lens (\CanInterface' {protocolName} -> protocolName) (\s@CanInterface' {} a -> s {protocolName = a} :: CanInterface)

-- | The version of the communication protocol for the interface.
canInterface_protocolVersion :: Lens.Lens' CanInterface (Prelude.Maybe Prelude.Text)
canInterface_protocolVersion = Lens.lens (\CanInterface' {protocolVersion} -> protocolVersion) (\s@CanInterface' {} a -> s {protocolVersion = a} :: CanInterface)

-- | The unique name of the interface.
canInterface_name :: Lens.Lens' CanInterface Prelude.Text
canInterface_name = Lens.lens (\CanInterface' {name} -> name) (\s@CanInterface' {} a -> s {name = a} :: CanInterface)

instance Data.FromJSON CanInterface where
  parseJSON =
    Data.withObject
      "CanInterface"
      ( \x ->
          CanInterface'
            Prelude.<$> (x Data..:? "protocolName")
            Prelude.<*> (x Data..:? "protocolVersion")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable CanInterface where
  hashWithSalt _salt CanInterface' {..} =
    _salt `Prelude.hashWithSalt` protocolName
      `Prelude.hashWithSalt` protocolVersion
      `Prelude.hashWithSalt` name

instance Prelude.NFData CanInterface where
  rnf CanInterface' {..} =
    Prelude.rnf protocolName
      `Prelude.seq` Prelude.rnf protocolVersion
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON CanInterface where
  toJSON CanInterface' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("protocolName" Data..=) Prelude.<$> protocolName,
            ("protocolVersion" Data..=)
              Prelude.<$> protocolVersion,
            Prelude.Just ("name" Data..= name)
          ]
      )
