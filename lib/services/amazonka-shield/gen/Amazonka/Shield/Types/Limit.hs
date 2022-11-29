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
-- Module      : Amazonka.Shield.Types.Limit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.Limit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies how many protections of a given type you can create.
--
-- /See:/ 'newLimit' smart constructor.
data Limit = Limit'
  { -- | The type of protection.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of protections that can be created for the specified
    -- @Type@.
    max :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Limit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'limit_type' - The type of protection.
--
-- 'max', 'limit_max' - The maximum number of protections that can be created for the specified
-- @Type@.
newLimit ::
  Limit
newLimit =
  Limit'
    { type' = Prelude.Nothing,
      max = Prelude.Nothing
    }

-- | The type of protection.
limit_type :: Lens.Lens' Limit (Prelude.Maybe Prelude.Text)
limit_type = Lens.lens (\Limit' {type'} -> type') (\s@Limit' {} a -> s {type' = a} :: Limit)

-- | The maximum number of protections that can be created for the specified
-- @Type@.
limit_max :: Lens.Lens' Limit (Prelude.Maybe Prelude.Integer)
limit_max = Lens.lens (\Limit' {max} -> max) (\s@Limit' {} a -> s {max = a} :: Limit)

instance Core.FromJSON Limit where
  parseJSON =
    Core.withObject
      "Limit"
      ( \x ->
          Limit'
            Prelude.<$> (x Core..:? "Type") Prelude.<*> (x Core..:? "Max")
      )

instance Prelude.Hashable Limit where
  hashWithSalt _salt Limit' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` max

instance Prelude.NFData Limit where
  rnf Limit' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf max
