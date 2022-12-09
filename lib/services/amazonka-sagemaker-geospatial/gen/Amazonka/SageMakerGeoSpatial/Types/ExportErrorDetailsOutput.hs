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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetailsOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ExportErrorDetailsOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.ExportErrorType

-- |
--
-- /See:/ 'newExportErrorDetailsOutput' smart constructor.
data ExportErrorDetailsOutput = ExportErrorDetailsOutput'
  { message :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe ExportErrorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportErrorDetailsOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'exportErrorDetailsOutput_message' -
--
-- 'type'', 'exportErrorDetailsOutput_type' -
newExportErrorDetailsOutput ::
  ExportErrorDetailsOutput
newExportErrorDetailsOutput =
  ExportErrorDetailsOutput'
    { message =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- |
exportErrorDetailsOutput_message :: Lens.Lens' ExportErrorDetailsOutput (Prelude.Maybe Prelude.Text)
exportErrorDetailsOutput_message = Lens.lens (\ExportErrorDetailsOutput' {message} -> message) (\s@ExportErrorDetailsOutput' {} a -> s {message = a} :: ExportErrorDetailsOutput)

-- |
exportErrorDetailsOutput_type :: Lens.Lens' ExportErrorDetailsOutput (Prelude.Maybe ExportErrorType)
exportErrorDetailsOutput_type = Lens.lens (\ExportErrorDetailsOutput' {type'} -> type') (\s@ExportErrorDetailsOutput' {} a -> s {type' = a} :: ExportErrorDetailsOutput)

instance Data.FromJSON ExportErrorDetailsOutput where
  parseJSON =
    Data.withObject
      "ExportErrorDetailsOutput"
      ( \x ->
          ExportErrorDetailsOutput'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ExportErrorDetailsOutput where
  hashWithSalt _salt ExportErrorDetailsOutput' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ExportErrorDetailsOutput where
  rnf ExportErrorDetailsOutput' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'
