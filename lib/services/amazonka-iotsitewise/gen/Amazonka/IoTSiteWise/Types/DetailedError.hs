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
-- Module      : Amazonka.IoTSiteWise.Types.DetailedError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.DetailedError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.DetailedErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed error information.
--
-- /See:/ 'newDetailedError' smart constructor.
data DetailedError = DetailedError'
  { -- | The error code.
    code :: DetailedErrorCode,
    -- | The error message.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetailedError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'detailedError_code' - The error code.
--
-- 'message', 'detailedError_message' - The error message.
newDetailedError ::
  -- | 'code'
  DetailedErrorCode ->
  -- | 'message'
  Prelude.Text ->
  DetailedError
newDetailedError pCode_ pMessage_ =
  DetailedError' {code = pCode_, message = pMessage_}

-- | The error code.
detailedError_code :: Lens.Lens' DetailedError DetailedErrorCode
detailedError_code = Lens.lens (\DetailedError' {code} -> code) (\s@DetailedError' {} a -> s {code = a} :: DetailedError)

-- | The error message.
detailedError_message :: Lens.Lens' DetailedError Prelude.Text
detailedError_message = Lens.lens (\DetailedError' {message} -> message) (\s@DetailedError' {} a -> s {message = a} :: DetailedError)

instance Data.FromJSON DetailedError where
  parseJSON =
    Data.withObject
      "DetailedError"
      ( \x ->
          DetailedError'
            Prelude.<$> (x Data..: "code")
            Prelude.<*> (x Data..: "message")
      )

instance Prelude.Hashable DetailedError where
  hashWithSalt _salt DetailedError' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData DetailedError where
  rnf DetailedError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
