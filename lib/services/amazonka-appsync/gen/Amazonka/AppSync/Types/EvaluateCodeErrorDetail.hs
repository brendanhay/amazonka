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
-- Module      : Amazonka.AppSync.Types.EvaluateCodeErrorDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.EvaluateCodeErrorDetail where

import Amazonka.AppSync.Types.CodeError
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the list of errors from a code evaluation response.
--
-- /See:/ 'newEvaluateCodeErrorDetail' smart constructor.
data EvaluateCodeErrorDetail = EvaluateCodeErrorDetail'
  { -- | The error payload.
    message :: Prelude.Maybe Prelude.Text,
    -- | Contains the list of @CodeError@ objects.
    codeErrors :: Prelude.Maybe [CodeError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluateCodeErrorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'evaluateCodeErrorDetail_message' - The error payload.
--
-- 'codeErrors', 'evaluateCodeErrorDetail_codeErrors' - Contains the list of @CodeError@ objects.
newEvaluateCodeErrorDetail ::
  EvaluateCodeErrorDetail
newEvaluateCodeErrorDetail =
  EvaluateCodeErrorDetail'
    { message = Prelude.Nothing,
      codeErrors = Prelude.Nothing
    }

-- | The error payload.
evaluateCodeErrorDetail_message :: Lens.Lens' EvaluateCodeErrorDetail (Prelude.Maybe Prelude.Text)
evaluateCodeErrorDetail_message = Lens.lens (\EvaluateCodeErrorDetail' {message} -> message) (\s@EvaluateCodeErrorDetail' {} a -> s {message = a} :: EvaluateCodeErrorDetail)

-- | Contains the list of @CodeError@ objects.
evaluateCodeErrorDetail_codeErrors :: Lens.Lens' EvaluateCodeErrorDetail (Prelude.Maybe [CodeError])
evaluateCodeErrorDetail_codeErrors = Lens.lens (\EvaluateCodeErrorDetail' {codeErrors} -> codeErrors) (\s@EvaluateCodeErrorDetail' {} a -> s {codeErrors = a} :: EvaluateCodeErrorDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EvaluateCodeErrorDetail where
  parseJSON =
    Data.withObject
      "EvaluateCodeErrorDetail"
      ( \x ->
          EvaluateCodeErrorDetail'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "codeErrors" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable EvaluateCodeErrorDetail where
  hashWithSalt _salt EvaluateCodeErrorDetail' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` codeErrors

instance Prelude.NFData EvaluateCodeErrorDetail where
  rnf EvaluateCodeErrorDetail' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf codeErrors
