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
-- Module      : Amazonka.HoneyCode.Types.ImportOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.ImportOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.HoneyCode.Types.DelimitedTextImportOptions
import Amazonka.HoneyCode.Types.DestinationOptions
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the options specified by the sumitter of the
-- import request.
--
-- /See:/ 'newImportOptions' smart constructor.
data ImportOptions = ImportOptions'
  { -- | Options relating to the destination of the import request.
    destinationOptions :: Prelude.Maybe DestinationOptions,
    -- | Options relating to parsing delimited text. Required if dataFormat is
    -- DELIMITED_TEXT.
    delimitedTextOptions :: Prelude.Maybe DelimitedTextImportOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationOptions', 'importOptions_destinationOptions' - Options relating to the destination of the import request.
--
-- 'delimitedTextOptions', 'importOptions_delimitedTextOptions' - Options relating to parsing delimited text. Required if dataFormat is
-- DELIMITED_TEXT.
newImportOptions ::
  ImportOptions
newImportOptions =
  ImportOptions'
    { destinationOptions =
        Prelude.Nothing,
      delimitedTextOptions = Prelude.Nothing
    }

-- | Options relating to the destination of the import request.
importOptions_destinationOptions :: Lens.Lens' ImportOptions (Prelude.Maybe DestinationOptions)
importOptions_destinationOptions = Lens.lens (\ImportOptions' {destinationOptions} -> destinationOptions) (\s@ImportOptions' {} a -> s {destinationOptions = a} :: ImportOptions)

-- | Options relating to parsing delimited text. Required if dataFormat is
-- DELIMITED_TEXT.
importOptions_delimitedTextOptions :: Lens.Lens' ImportOptions (Prelude.Maybe DelimitedTextImportOptions)
importOptions_delimitedTextOptions = Lens.lens (\ImportOptions' {delimitedTextOptions} -> delimitedTextOptions) (\s@ImportOptions' {} a -> s {delimitedTextOptions = a} :: ImportOptions)

instance Core.FromJSON ImportOptions where
  parseJSON =
    Core.withObject
      "ImportOptions"
      ( \x ->
          ImportOptions'
            Prelude.<$> (x Core..:? "destinationOptions")
            Prelude.<*> (x Core..:? "delimitedTextOptions")
      )

instance Prelude.Hashable ImportOptions where
  hashWithSalt _salt ImportOptions' {..} =
    _salt `Prelude.hashWithSalt` destinationOptions
      `Prelude.hashWithSalt` delimitedTextOptions

instance Prelude.NFData ImportOptions where
  rnf ImportOptions' {..} =
    Prelude.rnf destinationOptions
      `Prelude.seq` Prelude.rnf delimitedTextOptions

instance Core.ToJSON ImportOptions where
  toJSON ImportOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("destinationOptions" Core..=)
              Prelude.<$> destinationOptions,
            ("delimitedTextOptions" Core..=)
              Prelude.<$> delimitedTextOptions
          ]
      )
