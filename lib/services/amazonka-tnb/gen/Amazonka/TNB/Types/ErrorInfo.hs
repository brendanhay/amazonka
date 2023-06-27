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
-- Module      : Amazonka.TNB.Types.ErrorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.ErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides error information.
--
-- /See:/ 'newErrorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { -- | Error cause.
    cause :: Prelude.Maybe Prelude.Text,
    -- | Error details.
    details :: Prelude.Maybe Prelude.Text
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
-- 'cause', 'errorInfo_cause' - Error cause.
--
-- 'details', 'errorInfo_details' - Error details.
newErrorInfo ::
  ErrorInfo
newErrorInfo =
  ErrorInfo'
    { cause = Prelude.Nothing,
      details = Prelude.Nothing
    }

-- | Error cause.
errorInfo_cause :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_cause = Lens.lens (\ErrorInfo' {cause} -> cause) (\s@ErrorInfo' {} a -> s {cause = a} :: ErrorInfo)

-- | Error details.
errorInfo_details :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_details = Lens.lens (\ErrorInfo' {details} -> details) (\s@ErrorInfo' {} a -> s {details = a} :: ErrorInfo)

instance Data.FromJSON ErrorInfo where
  parseJSON =
    Data.withObject
      "ErrorInfo"
      ( \x ->
          ErrorInfo'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "details")
      )

instance Prelude.Hashable ErrorInfo where
  hashWithSalt _salt ErrorInfo' {..} =
    _salt
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` details

instance Prelude.NFData ErrorInfo where
  rnf ErrorInfo' {..} =
    Prelude.rnf cause `Prelude.seq` Prelude.rnf details
