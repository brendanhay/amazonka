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
-- Module      : Amazonka.IoTSiteWise.Types.ErrorDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types.DetailedError
import Amazonka.IoTSiteWise.Types.ErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of an IoT SiteWise error.
--
-- /See:/ 'newErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | A list of detailed errors.
    details :: Prelude.Maybe [DetailedError],
    -- | The error code.
    code :: ErrorCode,
    -- | The error message.
    message :: Prelude.Text
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
-- 'details', 'errorDetails_details' - A list of detailed errors.
--
-- 'code', 'errorDetails_code' - The error code.
--
-- 'message', 'errorDetails_message' - The error message.
newErrorDetails ::
  -- | 'code'
  ErrorCode ->
  -- | 'message'
  Prelude.Text ->
  ErrorDetails
newErrorDetails pCode_ pMessage_ =
  ErrorDetails'
    { details = Prelude.Nothing,
      code = pCode_,
      message = pMessage_
    }

-- | A list of detailed errors.
errorDetails_details :: Lens.Lens' ErrorDetails (Prelude.Maybe [DetailedError])
errorDetails_details = Lens.lens (\ErrorDetails' {details} -> details) (\s@ErrorDetails' {} a -> s {details = a} :: ErrorDetails) Prelude.. Lens.mapping Lens.coerced

-- | The error code.
errorDetails_code :: Lens.Lens' ErrorDetails ErrorCode
errorDetails_code = Lens.lens (\ErrorDetails' {code} -> code) (\s@ErrorDetails' {} a -> s {code = a} :: ErrorDetails)

-- | The error message.
errorDetails_message :: Lens.Lens' ErrorDetails Prelude.Text
errorDetails_message = Lens.lens (\ErrorDetails' {message} -> message) (\s@ErrorDetails' {} a -> s {message = a} :: ErrorDetails)

instance Core.FromJSON ErrorDetails where
  parseJSON =
    Core.withObject
      "ErrorDetails"
      ( \x ->
          ErrorDetails'
            Prelude.<$> (x Core..:? "details" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "code")
            Prelude.<*> (x Core..: "message")
      )

instance Prelude.Hashable ErrorDetails where
  hashWithSalt _salt ErrorDetails' {..} =
    _salt `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ErrorDetails where
  rnf ErrorDetails' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
