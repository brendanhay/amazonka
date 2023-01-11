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
-- Module      : Amazonka.ServiceCatalog.Types.RecordError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.RecordError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The error code and description resulting from an operation.
--
-- /See:/ 'newRecordError' smart constructor.
data RecordError = RecordError'
  { -- | The numeric value of the error.
    code :: Prelude.Maybe Prelude.Text,
    -- | The description of the error.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'recordError_code' - The numeric value of the error.
--
-- 'description', 'recordError_description' - The description of the error.
newRecordError ::
  RecordError
newRecordError =
  RecordError'
    { code = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The numeric value of the error.
recordError_code :: Lens.Lens' RecordError (Prelude.Maybe Prelude.Text)
recordError_code = Lens.lens (\RecordError' {code} -> code) (\s@RecordError' {} a -> s {code = a} :: RecordError)

-- | The description of the error.
recordError_description :: Lens.Lens' RecordError (Prelude.Maybe Prelude.Text)
recordError_description = Lens.lens (\RecordError' {description} -> description) (\s@RecordError' {} a -> s {description = a} :: RecordError)

instance Data.FromJSON RecordError where
  parseJSON =
    Data.withObject
      "RecordError"
      ( \x ->
          RecordError'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Description")
      )

instance Prelude.Hashable RecordError where
  hashWithSalt _salt RecordError' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` description

instance Prelude.NFData RecordError where
  rnf RecordError' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf description
