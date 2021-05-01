{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.XRay.Types.UnprocessedStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.UnprocessedStatistics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Sampling statistics from a call to GetSamplingTargets that X-Ray could
-- not process.
--
-- /See:/ 'newUnprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { -- | The name of the sampling rule.
    ruleName :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'unprocessedStatistics_ruleName' - The name of the sampling rule.
--
-- 'message', 'unprocessedStatistics_message' - The error message.
--
-- 'errorCode', 'unprocessedStatistics_errorCode' - The error code.
newUnprocessedStatistics ::
  UnprocessedStatistics
newUnprocessedStatistics =
  UnprocessedStatistics'
    { ruleName = Prelude.Nothing,
      message = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The name of the sampling rule.
unprocessedStatistics_ruleName :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_ruleName = Lens.lens (\UnprocessedStatistics' {ruleName} -> ruleName) (\s@UnprocessedStatistics' {} a -> s {ruleName = a} :: UnprocessedStatistics)

-- | The error message.
unprocessedStatistics_message :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_message = Lens.lens (\UnprocessedStatistics' {message} -> message) (\s@UnprocessedStatistics' {} a -> s {message = a} :: UnprocessedStatistics)

-- | The error code.
unprocessedStatistics_errorCode :: Lens.Lens' UnprocessedStatistics (Prelude.Maybe Prelude.Text)
unprocessedStatistics_errorCode = Lens.lens (\UnprocessedStatistics' {errorCode} -> errorCode) (\s@UnprocessedStatistics' {} a -> s {errorCode = a} :: UnprocessedStatistics)

instance Prelude.FromJSON UnprocessedStatistics where
  parseJSON =
    Prelude.withObject
      "UnprocessedStatistics"
      ( \x ->
          UnprocessedStatistics'
            Prelude.<$> (x Prelude..:? "RuleName")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable UnprocessedStatistics

instance Prelude.NFData UnprocessedStatistics
