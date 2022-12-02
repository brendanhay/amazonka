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
-- Module      : Amazonka.IoTFleetWise.Types.CanDbcDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CanDbcDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configurations used to create a decoder manifest.
--
-- /See:/ 'newCanDbcDefinition' smart constructor.
data CanDbcDefinition = CanDbcDefinition'
  { -- | Pairs every signal specified in your vehicle model with a signal
    -- decoder.
    signalsMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Contains information about a network interface.
    networkInterface :: Prelude.Text,
    -- | A list of DBC files. You can upload only one DBC file for each network
    -- interface and specify up to five (inclusive) files in the list.
    canDbcFiles :: Prelude.NonEmpty Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanDbcDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signalsMap', 'canDbcDefinition_signalsMap' - Pairs every signal specified in your vehicle model with a signal
-- decoder.
--
-- 'networkInterface', 'canDbcDefinition_networkInterface' - Contains information about a network interface.
--
-- 'canDbcFiles', 'canDbcDefinition_canDbcFiles' - A list of DBC files. You can upload only one DBC file for each network
-- interface and specify up to five (inclusive) files in the list.
newCanDbcDefinition ::
  -- | 'networkInterface'
  Prelude.Text ->
  -- | 'canDbcFiles'
  Prelude.NonEmpty Prelude.ByteString ->
  CanDbcDefinition
newCanDbcDefinition pNetworkInterface_ pCanDbcFiles_ =
  CanDbcDefinition'
    { signalsMap = Prelude.Nothing,
      networkInterface = pNetworkInterface_,
      canDbcFiles = Lens.coerced Lens.# pCanDbcFiles_
    }

-- | Pairs every signal specified in your vehicle model with a signal
-- decoder.
canDbcDefinition_signalsMap :: Lens.Lens' CanDbcDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
canDbcDefinition_signalsMap = Lens.lens (\CanDbcDefinition' {signalsMap} -> signalsMap) (\s@CanDbcDefinition' {} a -> s {signalsMap = a} :: CanDbcDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about a network interface.
canDbcDefinition_networkInterface :: Lens.Lens' CanDbcDefinition Prelude.Text
canDbcDefinition_networkInterface = Lens.lens (\CanDbcDefinition' {networkInterface} -> networkInterface) (\s@CanDbcDefinition' {} a -> s {networkInterface = a} :: CanDbcDefinition)

-- | A list of DBC files. You can upload only one DBC file for each network
-- interface and specify up to five (inclusive) files in the list.
canDbcDefinition_canDbcFiles :: Lens.Lens' CanDbcDefinition (Prelude.NonEmpty Prelude.ByteString)
canDbcDefinition_canDbcFiles = Lens.lens (\CanDbcDefinition' {canDbcFiles} -> canDbcFiles) (\s@CanDbcDefinition' {} a -> s {canDbcFiles = a} :: CanDbcDefinition) Prelude.. Lens.coerced

instance Prelude.Hashable CanDbcDefinition where
  hashWithSalt _salt CanDbcDefinition' {..} =
    _salt `Prelude.hashWithSalt` signalsMap
      `Prelude.hashWithSalt` networkInterface
      `Prelude.hashWithSalt` canDbcFiles

instance Prelude.NFData CanDbcDefinition where
  rnf CanDbcDefinition' {..} =
    Prelude.rnf signalsMap
      `Prelude.seq` Prelude.rnf networkInterface
      `Prelude.seq` Prelude.rnf canDbcFiles

instance Data.ToJSON CanDbcDefinition where
  toJSON CanDbcDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("signalsMap" Data..=) Prelude.<$> signalsMap,
            Prelude.Just
              ("networkInterface" Data..= networkInterface),
            Prelude.Just ("canDbcFiles" Data..= canDbcFiles)
          ]
      )
