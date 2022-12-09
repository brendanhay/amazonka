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
-- Module      : Amazonka.IotTwinMaker.Types.ErrorDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | The error details.
--
-- /See:/ 'newErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | The error code.
    code :: Prelude.Maybe ErrorCode,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'errorDetails_code' - The error code.
--
-- 'message', 'errorDetails_message' - The error message.
newErrorDetails ::
  ErrorDetails
newErrorDetails =
  ErrorDetails'
    { code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
errorDetails_code :: Lens.Lens' ErrorDetails (Prelude.Maybe ErrorCode)
errorDetails_code = Lens.lens (\ErrorDetails' {code} -> code) (\s@ErrorDetails' {} a -> s {code = a} :: ErrorDetails)

-- | The error message.
errorDetails_message :: Lens.Lens' ErrorDetails (Prelude.Maybe Prelude.Text)
errorDetails_message = Lens.lens (\ErrorDetails' {message} -> message) (\s@ErrorDetails' {} a -> s {message = a} :: ErrorDetails)

instance Data.FromJSON ErrorDetails where
  parseJSON =
    Data.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable ErrorDetails where
  hashWithSalt _salt ErrorDetails' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ErrorDetails where
  rnf ErrorDetails' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
