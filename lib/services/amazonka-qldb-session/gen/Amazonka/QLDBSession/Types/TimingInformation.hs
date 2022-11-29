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
-- Module      : Amazonka.QLDBSession.Types.TimingInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.TimingInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains server-side performance information for a command. Amazon QLDB
-- captures timing information between the times when it receives the
-- request and when it sends the corresponding response.
--
-- /See:/ 'newTimingInformation' smart constructor.
data TimingInformation = TimingInformation'
  { -- | The amount of time that QLDB spent on processing the command, measured
    -- in milliseconds.
    processingTimeMilliseconds :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimingInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingTimeMilliseconds', 'timingInformation_processingTimeMilliseconds' - The amount of time that QLDB spent on processing the command, measured
-- in milliseconds.
newTimingInformation ::
  TimingInformation
newTimingInformation =
  TimingInformation'
    { processingTimeMilliseconds =
        Prelude.Nothing
    }

-- | The amount of time that QLDB spent on processing the command, measured
-- in milliseconds.
timingInformation_processingTimeMilliseconds :: Lens.Lens' TimingInformation (Prelude.Maybe Prelude.Integer)
timingInformation_processingTimeMilliseconds = Lens.lens (\TimingInformation' {processingTimeMilliseconds} -> processingTimeMilliseconds) (\s@TimingInformation' {} a -> s {processingTimeMilliseconds = a} :: TimingInformation)

instance Core.FromJSON TimingInformation where
  parseJSON =
    Core.withObject
      "TimingInformation"
      ( \x ->
          TimingInformation'
            Prelude.<$> (x Core..:? "ProcessingTimeMilliseconds")
      )

instance Prelude.Hashable TimingInformation where
  hashWithSalt _salt TimingInformation' {..} =
    _salt
      `Prelude.hashWithSalt` processingTimeMilliseconds

instance Prelude.NFData TimingInformation where
  rnf TimingInformation' {..} =
    Prelude.rnf processingTimeMilliseconds
