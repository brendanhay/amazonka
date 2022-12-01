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
-- Module      : Amazonka.Glue.Types.JoinColumn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JoinColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a column to be joined.
--
-- /See:/ 'newJoinColumn' smart constructor.
data JoinColumn = JoinColumn'
  { -- | The column to be joined.
    from :: Prelude.Text,
    -- | The key of the column to be joined.
    keys :: [[Prelude.Text]]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JoinColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'joinColumn_from' - The column to be joined.
--
-- 'keys', 'joinColumn_keys' - The key of the column to be joined.
newJoinColumn ::
  -- | 'from'
  Prelude.Text ->
  JoinColumn
newJoinColumn pFrom_ =
  JoinColumn' {from = pFrom_, keys = Prelude.mempty}

-- | The column to be joined.
joinColumn_from :: Lens.Lens' JoinColumn Prelude.Text
joinColumn_from = Lens.lens (\JoinColumn' {from} -> from) (\s@JoinColumn' {} a -> s {from = a} :: JoinColumn)

-- | The key of the column to be joined.
joinColumn_keys :: Lens.Lens' JoinColumn [[Prelude.Text]]
joinColumn_keys = Lens.lens (\JoinColumn' {keys} -> keys) (\s@JoinColumn' {} a -> s {keys = a} :: JoinColumn) Prelude.. Lens.coerced

instance Core.FromJSON JoinColumn where
  parseJSON =
    Core.withObject
      "JoinColumn"
      ( \x ->
          JoinColumn'
            Prelude.<$> (x Core..: "From")
            Prelude.<*> (x Core..:? "Keys" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable JoinColumn where
  hashWithSalt _salt JoinColumn' {..} =
    _salt `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` keys

instance Prelude.NFData JoinColumn where
  rnf JoinColumn' {..} =
    Prelude.rnf from `Prelude.seq` Prelude.rnf keys

instance Core.ToJSON JoinColumn where
  toJSON JoinColumn' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("From" Core..= from),
            Prelude.Just ("Keys" Core..= keys)
          ]
      )
