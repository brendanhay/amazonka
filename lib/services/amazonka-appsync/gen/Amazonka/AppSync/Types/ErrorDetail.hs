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
-- Module      : Amazonka.AppSync.Types.ErrorDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.ErrorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the list of errors generated. When using JavaScript, this will
-- apply to the request or response function evaluation.
--
-- /See:/ 'newErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { -- | The error payload.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'errorDetail_message' - The error payload.
newErrorDetail ::
  ErrorDetail
newErrorDetail =
  ErrorDetail' {message = Prelude.Nothing}

-- | The error payload.
errorDetail_message :: Lens.Lens' ErrorDetail (Prelude.Maybe Prelude.Text)
errorDetail_message = Lens.lens (\ErrorDetail' {message} -> message) (\s@ErrorDetail' {} a -> s {message = a} :: ErrorDetail)

instance Data.FromJSON ErrorDetail where
  parseJSON =
    Data.withObject
      "ErrorDetail"
      ( \x ->
          ErrorDetail' Prelude.<$> (x Data..:? "message")
      )

instance Prelude.Hashable ErrorDetail where
  hashWithSalt _salt ErrorDetail' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData ErrorDetail where
  rnf ErrorDetail' {..} = Prelude.rnf message
