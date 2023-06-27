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
-- Module      : Amazonka.VerifiedPermissions.Types.EvaluationErrorItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.EvaluationErrorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a description of an evaluation error.
--
-- This data type is used as a request parameter in the
-- <https://docs.aws.amazon.com/amazon-verified-permissions/latest/APIReference/API_IsAuthorized.html IsAuthorized>
-- and
-- <https://docs.aws.amazon.com/amazon-verified-permissions/latest/APIReference/API_IsAuthorizedWithToken.html IsAuthorizedWithToken>
-- operations.
--
-- /See:/ 'newEvaluationErrorItem' smart constructor.
data EvaluationErrorItem = EvaluationErrorItem'
  { -- | The error description.
    errorDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDescription', 'evaluationErrorItem_errorDescription' - The error description.
newEvaluationErrorItem ::
  -- | 'errorDescription'
  Prelude.Text ->
  EvaluationErrorItem
newEvaluationErrorItem pErrorDescription_ =
  EvaluationErrorItem'
    { errorDescription =
        pErrorDescription_
    }

-- | The error description.
evaluationErrorItem_errorDescription :: Lens.Lens' EvaluationErrorItem Prelude.Text
evaluationErrorItem_errorDescription = Lens.lens (\EvaluationErrorItem' {errorDescription} -> errorDescription) (\s@EvaluationErrorItem' {} a -> s {errorDescription = a} :: EvaluationErrorItem)

instance Data.FromJSON EvaluationErrorItem where
  parseJSON =
    Data.withObject
      "EvaluationErrorItem"
      ( \x ->
          EvaluationErrorItem'
            Prelude.<$> (x Data..: "errorDescription")
      )

instance Prelude.Hashable EvaluationErrorItem where
  hashWithSalt _salt EvaluationErrorItem' {..} =
    _salt `Prelude.hashWithSalt` errorDescription

instance Prelude.NFData EvaluationErrorItem where
  rnf EvaluationErrorItem' {..} =
    Prelude.rnf errorDescription
