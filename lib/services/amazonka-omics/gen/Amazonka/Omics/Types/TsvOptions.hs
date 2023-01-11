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
-- Module      : Amazonka.Omics.Types.TsvOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.TsvOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadOptions
import qualified Amazonka.Prelude as Prelude

-- | Formatting options for a TSV file.
--
-- /See:/ 'newTsvOptions' smart constructor.
data TsvOptions = TsvOptions'
  { -- | The file\'s read options.
    readOptions :: Prelude.Maybe ReadOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TsvOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readOptions', 'tsvOptions_readOptions' - The file\'s read options.
newTsvOptions ::
  TsvOptions
newTsvOptions =
  TsvOptions' {readOptions = Prelude.Nothing}

-- | The file\'s read options.
tsvOptions_readOptions :: Lens.Lens' TsvOptions (Prelude.Maybe ReadOptions)
tsvOptions_readOptions = Lens.lens (\TsvOptions' {readOptions} -> readOptions) (\s@TsvOptions' {} a -> s {readOptions = a} :: TsvOptions)

instance Data.FromJSON TsvOptions where
  parseJSON =
    Data.withObject
      "TsvOptions"
      ( \x ->
          TsvOptions' Prelude.<$> (x Data..:? "readOptions")
      )

instance Prelude.Hashable TsvOptions where
  hashWithSalt _salt TsvOptions' {..} =
    _salt `Prelude.hashWithSalt` readOptions

instance Prelude.NFData TsvOptions where
  rnf TsvOptions' {..} = Prelude.rnf readOptions

instance Data.ToJSON TsvOptions where
  toJSON TsvOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("readOptions" Data..=) Prelude.<$> readOptions]
      )
