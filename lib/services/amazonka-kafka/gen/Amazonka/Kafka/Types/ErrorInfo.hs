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
-- Module      : Amazonka.Kafka.Types.ErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about an error state of the cluster.
--
-- /See:/ 'newErrorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { -- | A number describing the error programmatically.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | An optional field to provide more details about the error.
    errorString :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'errorInfo_errorCode' - A number describing the error programmatically.
--
-- 'errorString', 'errorInfo_errorString' - An optional field to provide more details about the error.
newErrorInfo ::
  ErrorInfo
newErrorInfo =
  ErrorInfo'
    { errorCode = Prelude.Nothing,
      errorString = Prelude.Nothing
    }

-- | A number describing the error programmatically.
errorInfo_errorCode :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_errorCode = Lens.lens (\ErrorInfo' {errorCode} -> errorCode) (\s@ErrorInfo' {} a -> s {errorCode = a} :: ErrorInfo)

-- | An optional field to provide more details about the error.
errorInfo_errorString :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_errorString = Lens.lens (\ErrorInfo' {errorString} -> errorString) (\s@ErrorInfo' {} a -> s {errorString = a} :: ErrorInfo)

instance Data.FromJSON ErrorInfo where
  parseJSON =
    Data.withObject
      "ErrorInfo"
      ( \x ->
          ErrorInfo'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorString")
      )

instance Prelude.Hashable ErrorInfo where
  hashWithSalt _salt ErrorInfo' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorString

instance Prelude.NFData ErrorInfo where
  rnf ErrorInfo' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorString
