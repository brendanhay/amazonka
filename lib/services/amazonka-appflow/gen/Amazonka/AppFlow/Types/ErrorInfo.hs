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
-- Module      : Amazonka.AppFlow.Types.ErrorInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ErrorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides details in the event of a failed flow, including the failure
-- count and the related error messages.
--
-- /See:/ 'newErrorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { -- | Specifies the error message that appears if a flow fails.
    executionMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the failure count for the attempted flow.
    putFailuresCount :: Prelude.Maybe Prelude.Integer
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
-- 'executionMessage', 'errorInfo_executionMessage' - Specifies the error message that appears if a flow fails.
--
-- 'putFailuresCount', 'errorInfo_putFailuresCount' - Specifies the failure count for the attempted flow.
newErrorInfo ::
  ErrorInfo
newErrorInfo =
  ErrorInfo'
    { executionMessage = Prelude.Nothing,
      putFailuresCount = Prelude.Nothing
    }

-- | Specifies the error message that appears if a flow fails.
errorInfo_executionMessage :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Text)
errorInfo_executionMessage = Lens.lens (\ErrorInfo' {executionMessage} -> executionMessage) (\s@ErrorInfo' {} a -> s {executionMessage = a} :: ErrorInfo)

-- | Specifies the failure count for the attempted flow.
errorInfo_putFailuresCount :: Lens.Lens' ErrorInfo (Prelude.Maybe Prelude.Integer)
errorInfo_putFailuresCount = Lens.lens (\ErrorInfo' {putFailuresCount} -> putFailuresCount) (\s@ErrorInfo' {} a -> s {putFailuresCount = a} :: ErrorInfo)

instance Core.FromJSON ErrorInfo where
  parseJSON =
    Core.withObject
      "ErrorInfo"
      ( \x ->
          ErrorInfo'
            Prelude.<$> (x Core..:? "executionMessage")
            Prelude.<*> (x Core..:? "putFailuresCount")
      )

instance Prelude.Hashable ErrorInfo where
  hashWithSalt _salt ErrorInfo' {..} =
    _salt `Prelude.hashWithSalt` executionMessage
      `Prelude.hashWithSalt` putFailuresCount

instance Prelude.NFData ErrorInfo where
  rnf ErrorInfo' {..} =
    Prelude.rnf executionMessage
      `Prelude.seq` Prelude.rnf putFailuresCount
