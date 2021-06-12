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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Sampling statistics from a call to GetSamplingTargets that X-Ray could
-- not process.
--
-- /See:/ 'newUnprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { -- | The name of the sampling rule.
    ruleName :: Core.Maybe Core.Text,
    -- | The error message.
    message :: Core.Maybe Core.Text,
    -- | The error code.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { ruleName = Core.Nothing,
      message = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The name of the sampling rule.
unprocessedStatistics_ruleName :: Lens.Lens' UnprocessedStatistics (Core.Maybe Core.Text)
unprocessedStatistics_ruleName = Lens.lens (\UnprocessedStatistics' {ruleName} -> ruleName) (\s@UnprocessedStatistics' {} a -> s {ruleName = a} :: UnprocessedStatistics)

-- | The error message.
unprocessedStatistics_message :: Lens.Lens' UnprocessedStatistics (Core.Maybe Core.Text)
unprocessedStatistics_message = Lens.lens (\UnprocessedStatistics' {message} -> message) (\s@UnprocessedStatistics' {} a -> s {message = a} :: UnprocessedStatistics)

-- | The error code.
unprocessedStatistics_errorCode :: Lens.Lens' UnprocessedStatistics (Core.Maybe Core.Text)
unprocessedStatistics_errorCode = Lens.lens (\UnprocessedStatistics' {errorCode} -> errorCode) (\s@UnprocessedStatistics' {} a -> s {errorCode = a} :: UnprocessedStatistics)

instance Core.FromJSON UnprocessedStatistics where
  parseJSON =
    Core.withObject
      "UnprocessedStatistics"
      ( \x ->
          UnprocessedStatistics'
            Core.<$> (x Core..:? "RuleName")
            Core.<*> (x Core..:? "Message")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable UnprocessedStatistics

instance Core.NFData UnprocessedStatistics
