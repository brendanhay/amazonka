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
-- Module      : Amazonka.AppMesh.Types.LoggingFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.LoggingFormat where

import Amazonka.AppMesh.Types.JsonFormatRef
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the format for the logs.
--
-- /See:/ 'newLoggingFormat' smart constructor.
data LoggingFormat = LoggingFormat'
  { json :: Prelude.Maybe [JsonFormatRef],
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'json', 'loggingFormat_json' -
--
-- 'text', 'loggingFormat_text' -
newLoggingFormat ::
  LoggingFormat
newLoggingFormat =
  LoggingFormat'
    { json = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- |
loggingFormat_json :: Lens.Lens' LoggingFormat (Prelude.Maybe [JsonFormatRef])
loggingFormat_json = Lens.lens (\LoggingFormat' {json} -> json) (\s@LoggingFormat' {} a -> s {json = a} :: LoggingFormat) Prelude.. Lens.mapping Lens.coerced

-- |
loggingFormat_text :: Lens.Lens' LoggingFormat (Prelude.Maybe Prelude.Text)
loggingFormat_text = Lens.lens (\LoggingFormat' {text} -> text) (\s@LoggingFormat' {} a -> s {text = a} :: LoggingFormat)

instance Data.FromJSON LoggingFormat where
  parseJSON =
    Data.withObject
      "LoggingFormat"
      ( \x ->
          LoggingFormat'
            Prelude.<$> (x Data..:? "json" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "text")
      )

instance Prelude.Hashable LoggingFormat where
  hashWithSalt _salt LoggingFormat' {..} =
    _salt `Prelude.hashWithSalt` json
      `Prelude.hashWithSalt` text

instance Prelude.NFData LoggingFormat where
  rnf LoggingFormat' {..} =
    Prelude.rnf json `Prelude.seq` Prelude.rnf text

instance Data.ToJSON LoggingFormat where
  toJSON LoggingFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("json" Data..=) Prelude.<$> json,
            ("text" Data..=) Prelude.<$> text
          ]
      )
