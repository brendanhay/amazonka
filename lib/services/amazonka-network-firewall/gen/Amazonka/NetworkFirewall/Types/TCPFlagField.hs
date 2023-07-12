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
-- Module      : Amazonka.NetworkFirewall.Types.TCPFlagField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.TCPFlagField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.TCPFlag
import qualified Amazonka.Prelude as Prelude

-- | TCP flags and masks to inspect packets for, used in stateless rules
-- MatchAttributes settings.
--
-- /See:/ 'newTCPFlagField' smart constructor.
data TCPFlagField = TCPFlagField'
  { -- | The set of flags to consider in the inspection. To inspect all flags in
    -- the valid values list, leave this with no setting.
    masks :: Prelude.Maybe [TCPFlag],
    -- | Used in conjunction with the @Masks@ setting to define the flags that
    -- must be set and flags that must not be set in order for the packet to
    -- match. This setting can only specify values that are also specified in
    -- the @Masks@ setting.
    --
    -- For the flags that are specified in the masks setting, the following
    -- must be true for the packet to match:
    --
    -- -   The ones that are set in this flags setting must be set in the
    --     packet.
    --
    -- -   The ones that are not set in this flags setting must also not be set
    --     in the packet.
    flags :: [TCPFlag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TCPFlagField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masks', 'tCPFlagField_masks' - The set of flags to consider in the inspection. To inspect all flags in
-- the valid values list, leave this with no setting.
--
-- 'flags', 'tCPFlagField_flags' - Used in conjunction with the @Masks@ setting to define the flags that
-- must be set and flags that must not be set in order for the packet to
-- match. This setting can only specify values that are also specified in
-- the @Masks@ setting.
--
-- For the flags that are specified in the masks setting, the following
-- must be true for the packet to match:
--
-- -   The ones that are set in this flags setting must be set in the
--     packet.
--
-- -   The ones that are not set in this flags setting must also not be set
--     in the packet.
newTCPFlagField ::
  TCPFlagField
newTCPFlagField =
  TCPFlagField'
    { masks = Prelude.Nothing,
      flags = Prelude.mempty
    }

-- | The set of flags to consider in the inspection. To inspect all flags in
-- the valid values list, leave this with no setting.
tCPFlagField_masks :: Lens.Lens' TCPFlagField (Prelude.Maybe [TCPFlag])
tCPFlagField_masks = Lens.lens (\TCPFlagField' {masks} -> masks) (\s@TCPFlagField' {} a -> s {masks = a} :: TCPFlagField) Prelude.. Lens.mapping Lens.coerced

-- | Used in conjunction with the @Masks@ setting to define the flags that
-- must be set and flags that must not be set in order for the packet to
-- match. This setting can only specify values that are also specified in
-- the @Masks@ setting.
--
-- For the flags that are specified in the masks setting, the following
-- must be true for the packet to match:
--
-- -   The ones that are set in this flags setting must be set in the
--     packet.
--
-- -   The ones that are not set in this flags setting must also not be set
--     in the packet.
tCPFlagField_flags :: Lens.Lens' TCPFlagField [TCPFlag]
tCPFlagField_flags = Lens.lens (\TCPFlagField' {flags} -> flags) (\s@TCPFlagField' {} a -> s {flags = a} :: TCPFlagField) Prelude.. Lens.coerced

instance Data.FromJSON TCPFlagField where
  parseJSON =
    Data.withObject
      "TCPFlagField"
      ( \x ->
          TCPFlagField'
            Prelude.<$> (x Data..:? "Masks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Flags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TCPFlagField where
  hashWithSalt _salt TCPFlagField' {..} =
    _salt
      `Prelude.hashWithSalt` masks
      `Prelude.hashWithSalt` flags

instance Prelude.NFData TCPFlagField where
  rnf TCPFlagField' {..} =
    Prelude.rnf masks `Prelude.seq` Prelude.rnf flags

instance Data.ToJSON TCPFlagField where
  toJSON TCPFlagField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Masks" Data..=) Prelude.<$> masks,
            Prelude.Just ("Flags" Data..= flags)
          ]
      )
