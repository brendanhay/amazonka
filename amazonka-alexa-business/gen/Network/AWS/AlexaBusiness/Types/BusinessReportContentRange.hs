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
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportContentRange where

import Network.AWS.AlexaBusiness.Types.BusinessReportInterval
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The content range of the report.
--
-- /See:/ 'newBusinessReportContentRange' smart constructor.
data BusinessReportContentRange = BusinessReportContentRange'
  { -- | The interval of the content range.
    interval :: BusinessReportInterval
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON BusinessReportContentRange where
  parseJSON =
    Prelude.withObject
      "BusinessReportContentRange"
      ( \x ->
          BusinessReportContentRange'
            Prelude.<$> (x Prelude..: "Interval")
      )

instance Prelude.Hashable BusinessReportContentRange

instance Prelude.NFData BusinessReportContentRange

instance Prelude.ToJSON BusinessReportContentRange where
  toJSON BusinessReportContentRange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Interval" Prelude..= interval)]
      )
