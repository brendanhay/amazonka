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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.ImportOptions where

import qualified Amazonka.Core as Core
import Amazonka.HoneyCode.Types.DelimitedTextImportOptions
import Amazonka.HoneyCode.Types.DestinationOptions
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the options specified by the sumitter of the
-- import request.
--
-- /See:/ 'newImportOptions' smart constructor.
data ImportOptions = ImportOptions'
  { -- | Options relating to parsing delimited text. Required if dataFormat is
    -- DELIMITED_TEXT.
    delimitedTextOptions :: Prelude.Maybe DelimitedTextImportOptions,
    -- | Options relating to the destination of the import request.
    destinationOptions :: Prelude.Maybe DestinationOptions
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
-- 'delimitedTextOptions', 'importOptions_delimitedTextOptions' - Options relating to parsing delimited text. Required if dataFormat is
-- DELIMITED_TEXT.
--
-- 'destinationOptions', 'importOptions_destinationOptions' - Options relating to the destination of the import request.
newImportOptions ::
  ImportOptions
newImportOptions =
  ImportOptions'
    { delimitedTextOptions =
        Prelude.Nothing,
      destinationOptions = Prelude.Nothing
    }

-- | Options relating to parsing delimited text. Required if dataFormat is
-- DELIMITED_TEXT.
importOptions_delimitedTextOptions :: Lens.Lens' ImportOptions (Prelude.Maybe DelimitedTextImportOptions)
importOptions_delimitedTextOptions = Lens.lens (\ImportOptions' {delimitedTextOptions} -> delimitedTextOptions) (\s@ImportOptions' {} a -> s {delimitedTextOptions = a} :: ImportOptions)

-- | Options relating to the destination of the import request.
importOptions_destinationOptions :: Lens.Lens' ImportOptions (Prelude.Maybe DestinationOptions)
importOptions_destinationOptions = Lens.lens (\ImportOptions' {destinationOptions} -> destinationOptions) (\s@ImportOptions' {} a -> s {destinationOptions = a} :: ImportOptions)

instance Core.FromJSON ImportOptions where
  parseJSON =
    Core.withObject
      "ImportOptions"
      ( \x ->
          ImportOptions'
            Prelude.<$> (x Core..:? "delimitedTextOptions")
            Prelude.<*> (x Core..:? "destinationOptions")
      )

instance Prelude.Hashable ImportOptions

instance Prelude.NFData ImportOptions

instance Core.ToJSON ImportOptions where
  toJSON ImportOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("delimitedTextOptions" Core..=)
              Prelude.<$> delimitedTextOptions,
            ("destinationOptions" Core..=)
              Prelude.<$> destinationOptions
          ]
      )
