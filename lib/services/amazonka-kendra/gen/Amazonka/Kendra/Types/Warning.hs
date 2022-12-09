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
-- Module      : Amazonka.Kendra.Types.Warning
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Warning where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.WarningCode
import qualified Amazonka.Prelude as Prelude

-- | The warning code and message that explains a problem with a query.
--
-- /See:/ 'newWarning' smart constructor.
data Warning = Warning'
  { -- | The code used to show the type of warning for the query.
    code :: Prelude.Maybe WarningCode,
    -- | The message that explains the problem with the query.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Warning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'warning_code' - The code used to show the type of warning for the query.
--
-- 'message', 'warning_message' - The message that explains the problem with the query.
newWarning ::
  Warning
newWarning =
  Warning'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The code used to show the type of warning for the query.
warning_code :: Lens.Lens' Warning (Prelude.Maybe WarningCode)
warning_code = Lens.lens (\Warning' {code} -> code) (\s@Warning' {} a -> s {code = a} :: Warning)

-- | The message that explains the problem with the query.
warning_message :: Lens.Lens' Warning (Prelude.Maybe Prelude.Text)
warning_message = Lens.lens (\Warning' {message} -> message) (\s@Warning' {} a -> s {message = a} :: Warning)

instance Data.FromJSON Warning where
  parseJSON =
    Data.withObject
      "Warning"
      ( \x ->
          Warning'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable Warning where
  hashWithSalt _salt Warning' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData Warning where
  rnf Warning' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
