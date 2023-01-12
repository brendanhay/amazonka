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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.UnprocessedStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sampling statistics from a call to
-- <https://docs.aws.amazon.com/xray/latest/api/API_GetSamplingTargets.html GetSamplingTargets>
-- that X-Ray could not process.
--
-- /See:/ 'newUnprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the sampling rule.
    ruleName :: Prelude.Maybe Prelude.Text
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
-- 'errorCode', 'unprocessedStatistics_errorCode' - The error code.
--
-- 'message', 'unprocessedStatistics_message' - The error message.
--
-- 'ruleName', 'unprocessedStatistics_ruleName' - The name of the sampling rule.
newUnprocessedStatistics ::
  UnprocessedStatistics
newUnprocessedStatistics =
  UnprocessedStatistics'
    { errorCode = Prelude.Nothing,
      message = Prelude.Nothing,
      ruleName = Prelude.Nothing
    }

-- | The error code.
unprocessedStatistics_errorCode :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_errorCode = Lens.lens (\UnprocessedStatistics' {errorCode} -> errorCode) (\s@UnprocessedStatistics' {} a -> s {errorCode = a} :: UnprocessedStatistics)

-- | The error message.
unprocessedStatistics_message :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_message = Lens.lens (\UnprocessedStatistics' {message} -> message) (\s@UnprocessedStatistics' {} a -> s {message = a} :: UnprocessedStatistics)

-- | The name of the sampling rule.
unprocessedStatistics_ruleName :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_ruleName = Lens.lens (\UnprocessedStatistics' {ruleName} -> ruleName) (\s@UnprocessedStatistics' {} a -> s {ruleName = a} :: UnprocessedStatistics)

instance Data.FromJSON UnprocessedStatistics where
  parseJSON =
    Data.withObject
      "UnprocessedStatistics"
      ( \x ->
          UnprocessedStatistics'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "RuleName")
      )

instance Prelude.Hashable UnprocessedStatistics where
  hashWithSalt _salt UnprocessedStatistics' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` ruleName

instance Prelude.NFData UnprocessedStatistics where
  rnf UnprocessedStatistics' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf ruleName
