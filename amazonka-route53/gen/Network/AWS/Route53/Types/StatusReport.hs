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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal

-- | A complex type that contains the status that one Amazon Route 53 health
-- checker reports and the time of the health check.
--
-- /See:/ 'newStatusReport' smart constructor.
data StatusReport = StatusReport'
  { -- | A description of the status of the health check endpoint as reported by
    -- one of the Amazon Route 53 health checkers.
    status :: Core.Maybe Core.Text,
    -- | The date and time that the health checker performed the health check in
    -- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
    -- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
    -- represents March 27, 2017 at 17:48:16.751 UTC.
    checkedTime :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      checkedTime = Core.Nothing
    }

-- | A description of the status of the health check endpoint as reported by
-- one of the Amazon Route 53 health checkers.
statusReport_status :: Lens.Lens' StatusReport (Core.Maybe Core.Text)
statusReport_status = Lens.lens (\StatusReport' {status} -> status) (\s@StatusReport' {} a -> s {status = a} :: StatusReport)

-- | The date and time that the health checker performed the health check in
-- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
-- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
-- represents March 27, 2017 at 17:48:16.751 UTC.
statusReport_checkedTime :: Lens.Lens' StatusReport (Core.Maybe Core.UTCTime)
statusReport_checkedTime = Lens.lens (\StatusReport' {checkedTime} -> checkedTime) (\s@StatusReport' {} a -> s {checkedTime = a} :: StatusReport) Core.. Lens.mapping Core._Time

instance Core.FromXML StatusReport where
  parseXML x =
    StatusReport'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "CheckedTime")

instance Core.Hashable StatusReport

instance Core.NFData StatusReport
