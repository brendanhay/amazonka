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
-- Module      : Network.AWS.Route53.Types.StatusReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.StatusReport where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

-- | A complex type that contains the status that one Amazon Route 53 health
-- checker reports and the time of the health check.
--
-- /See:/ 'newStatusReport' smart constructor.
data StatusReport = StatusReport'
  { -- | A description of the status of the health check endpoint as reported by
    -- one of the Amazon Route 53 health checkers.
    status :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the health checker performed the health check in
    -- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
    -- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
    -- represents March 27, 2017 at 17:48:16.751 UTC.
    checkedTime :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StatusReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'statusReport_status' - A description of the status of the health check endpoint as reported by
-- one of the Amazon Route 53 health checkers.
--
-- 'checkedTime', 'statusReport_checkedTime' - The date and time that the health checker performed the health check in
-- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
-- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
-- represents March 27, 2017 at 17:48:16.751 UTC.
newStatusReport ::
  StatusReport
newStatusReport =
  StatusReport'
    { status = Prelude.Nothing,
      checkedTime = Prelude.Nothing
    }

-- | A description of the status of the health check endpoint as reported by
-- one of the Amazon Route 53 health checkers.
statusReport_status :: Lens.Lens' StatusReport (Prelude.Maybe Prelude.Text)
statusReport_status = Lens.lens (\StatusReport' {status} -> status) (\s@StatusReport' {} a -> s {status = a} :: StatusReport)

-- | The date and time that the health checker performed the health check in
-- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
-- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
-- represents March 27, 2017 at 17:48:16.751 UTC.
statusReport_checkedTime :: Lens.Lens' StatusReport (Prelude.Maybe Prelude.UTCTime)
statusReport_checkedTime = Lens.lens (\StatusReport' {checkedTime} -> checkedTime) (\s@StatusReport' {} a -> s {checkedTime = a} :: StatusReport) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML StatusReport where
  parseXML x =
    StatusReport'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "CheckedTime")

instance Prelude.Hashable StatusReport

instance Prelude.NFData StatusReport
