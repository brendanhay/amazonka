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
-- Module      : Amazonka.AlexaBusiness.Types.BusinessReportContentRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.BusinessReportContentRange where

import Amazonka.AlexaBusiness.Types.BusinessReportInterval
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The content range of the report.
--
-- /See:/ 'newBusinessReportContentRange' smart constructor.
data BusinessReportContentRange = BusinessReportContentRange'
  { -- | The interval of the content range.
    interval :: BusinessReportInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BusinessReportContentRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'interval', 'businessReportContentRange_interval' - The interval of the content range.
newBusinessReportContentRange ::
  -- | 'interval'
  BusinessReportInterval ->
  BusinessReportContentRange
newBusinessReportContentRange pInterval_ =
  BusinessReportContentRange' {interval = pInterval_}

-- | The interval of the content range.
businessReportContentRange_interval :: Lens.Lens' BusinessReportContentRange BusinessReportInterval
businessReportContentRange_interval = Lens.lens (\BusinessReportContentRange' {interval} -> interval) (\s@BusinessReportContentRange' {} a -> s {interval = a} :: BusinessReportContentRange)

instance Core.FromJSON BusinessReportContentRange where
  parseJSON =
    Core.withObject
      "BusinessReportContentRange"
      ( \x ->
          BusinessReportContentRange'
            Prelude.<$> (x Core..: "Interval")
      )

instance Prelude.Hashable BusinessReportContentRange where
  hashWithSalt _salt BusinessReportContentRange' {..} =
    _salt `Prelude.hashWithSalt` interval

instance Prelude.NFData BusinessReportContentRange where
  rnf BusinessReportContentRange' {..} =
    Prelude.rnf interval

instance Core.ToJSON BusinessReportContentRange where
  toJSON BusinessReportContentRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Interval" Core..= interval)]
      )
