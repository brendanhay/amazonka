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
-- Module      : Amazonka.ComputeOptimizer.Types.GetRecommendationError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.GetRecommendationError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an error experienced when getting recommendations.
--
-- For example, an error is returned if you request recommendations for an
-- unsupported Auto Scaling group, or if you request recommendations for an
-- instance of an unsupported instance family.
--
-- /See:/ 'newGetRecommendationError' smart constructor.
data GetRecommendationError = GetRecommendationError'
  { -- | The message, or reason, for the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    code :: Prelude.Maybe Prelude.Text,
    -- | The ID of the error.
    identifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendationError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'getRecommendationError_message' - The message, or reason, for the error.
--
-- 'code', 'getRecommendationError_code' - The error code.
--
-- 'identifier', 'getRecommendationError_identifier' - The ID of the error.
newGetRecommendationError ::
  GetRecommendationError
newGetRecommendationError =
  GetRecommendationError'
    { message = Prelude.Nothing,
      code = Prelude.Nothing,
      identifier = Prelude.Nothing
    }

-- | The message, or reason, for the error.
getRecommendationError_message :: Lens.Lens' GetRecommendationError (Prelude.Maybe Prelude.Text)
getRecommendationError_message = Lens.lens (\GetRecommendationError' {message} -> message) (\s@GetRecommendationError' {} a -> s {message = a} :: GetRecommendationError)

-- | The error code.
getRecommendationError_code :: Lens.Lens' GetRecommendationError (Prelude.Maybe Prelude.Text)
getRecommendationError_code = Lens.lens (\GetRecommendationError' {code} -> code) (\s@GetRecommendationError' {} a -> s {code = a} :: GetRecommendationError)

-- | The ID of the error.
getRecommendationError_identifier :: Lens.Lens' GetRecommendationError (Prelude.Maybe Prelude.Text)
getRecommendationError_identifier = Lens.lens (\GetRecommendationError' {identifier} -> identifier) (\s@GetRecommendationError' {} a -> s {identifier = a} :: GetRecommendationError)

instance Core.FromJSON GetRecommendationError where
  parseJSON =
    Core.withObject
      "GetRecommendationError"
      ( \x ->
          GetRecommendationError'
            Prelude.<$> (x Core..:? "message")
            Prelude.<*> (x Core..:? "code")
            Prelude.<*> (x Core..:? "identifier")
      )

instance Prelude.Hashable GetRecommendationError where
  hashWithSalt _salt GetRecommendationError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetRecommendationError where
  rnf GetRecommendationError' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf identifier
