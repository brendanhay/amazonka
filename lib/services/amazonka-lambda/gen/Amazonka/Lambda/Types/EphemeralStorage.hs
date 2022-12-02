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
-- Module      : Amazonka.Lambda.Types.EphemeralStorage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.EphemeralStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The size of the function’s \/tmp directory in MB. The default value is
-- 512, but can be any whole number between 512 and 10240 MB.
--
-- /See:/ 'newEphemeralStorage' smart constructor.
data EphemeralStorage = EphemeralStorage'
  { -- | The size of the function’s \/tmp directory.
    size :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EphemeralStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'ephemeralStorage_size' - The size of the function’s \/tmp directory.
newEphemeralStorage ::
  -- | 'size'
  Prelude.Natural ->
  EphemeralStorage
newEphemeralStorage pSize_ =
  EphemeralStorage' {size = pSize_}

-- | The size of the function’s \/tmp directory.
ephemeralStorage_size :: Lens.Lens' EphemeralStorage Prelude.Natural
ephemeralStorage_size = Lens.lens (\EphemeralStorage' {size} -> size) (\s@EphemeralStorage' {} a -> s {size = a} :: EphemeralStorage)

instance Data.FromJSON EphemeralStorage where
  parseJSON =
    Data.withObject
      "EphemeralStorage"
      ( \x ->
          EphemeralStorage' Prelude.<$> (x Data..: "Size")
      )

instance Prelude.Hashable EphemeralStorage where
  hashWithSalt _salt EphemeralStorage' {..} =
    _salt `Prelude.hashWithSalt` size

instance Prelude.NFData EphemeralStorage where
  rnf EphemeralStorage' {..} = Prelude.rnf size

instance Data.ToJSON EphemeralStorage where
  toJSON EphemeralStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Size" Data..= size)]
      )
