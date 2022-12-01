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
-- Module      : Amazonka.XRay.Types.UnprocessedStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.UnprocessedStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Sampling statistics from a call to
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingTargets.html GetSamplingTargets>
-- that X-Ray could not process.
--
-- /See:/ 'newUnprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the sampling rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'unprocessedStatistics_message' - The error message.
--
-- 'ruleName', 'unprocessedStatistics_ruleName' - The name of the sampling rule.
--
-- 'errorCode', 'unprocessedStatistics_errorCode' - The error code.
newUnprocessedStatistics ::
  UnprocessedStatistics
newUnprocessedStatistics =
  UnprocessedStatistics'
    { message = Prelude.Nothing,
      ruleName = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message.
unprocessedStatistics_message :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_message = Lens.lens (\UnprocessedStatistics' {message} -> message) (\s@UnprocessedStatistics' {} a -> s {message = a} :: UnprocessedStatistics)

-- | The name of the sampling rule.
unprocessedStatistics_ruleName :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_ruleName = Lens.lens (\UnprocessedStatistics' {ruleName} -> ruleName) (\s@UnprocessedStatistics' {} a -> s {ruleName = a} :: UnprocessedStatistics)

-- | The error code.
unprocessedStatistics_errorCode :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_errorCode = Lens.lens (\UnprocessedStatistics' {errorCode} -> errorCode) (\s@UnprocessedStatistics' {} a -> s {errorCode = a} :: UnprocessedStatistics)

instance Core.FromJSON UnprocessedStatistics where
  parseJSON =
    Core.withObject
      "UnprocessedStatistics"
      ( \x ->
          UnprocessedStatistics'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "RuleName")
            Prelude.<*> (x Core..:? "ErrorCode")
      )

instance Prelude.Hashable UnprocessedStatistics where
  hashWithSalt _salt UnprocessedStatistics' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData UnprocessedStatistics where
  rnf UnprocessedStatistics' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf errorCode
