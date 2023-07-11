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
-- Module      : Amazonka.Route53.Types.StatusReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.StatusReport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that contains the status that one Amazon Route 53 health
-- checker reports and the time of the health check.
--
-- /See:/ 'newStatusReport' smart constructor.
data StatusReport = StatusReport'
  { -- | The date and time that the health checker performed the health check in
    -- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
    -- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
    -- represents March 27, 2017 at 17:48:16.751 UTC.
    checkedTime :: Prelude.Maybe Data.ISO8601,
    -- | A description of the status of the health check endpoint as reported by
    -- one of the Amazon Route 53 health checkers.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatusReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkedTime', 'statusReport_checkedTime' - The date and time that the health checker performed the health check in
-- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
-- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
-- represents March 27, 2017 at 17:48:16.751 UTC.
--
-- 'status', 'statusReport_status' - A description of the status of the health check endpoint as reported by
-- one of the Amazon Route 53 health checkers.
newStatusReport ::
  StatusReport
newStatusReport =
  StatusReport'
    { checkedTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time that the health checker performed the health check in
-- <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated
-- Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@
-- represents March 27, 2017 at 17:48:16.751 UTC.
statusReport_checkedTime :: Lens.Lens' StatusReport (Prelude.Maybe Prelude.UTCTime)
statusReport_checkedTime = Lens.lens (\StatusReport' {checkedTime} -> checkedTime) (\s@StatusReport' {} a -> s {checkedTime = a} :: StatusReport) Prelude.. Lens.mapping Data._Time

-- | A description of the status of the health check endpoint as reported by
-- one of the Amazon Route 53 health checkers.
statusReport_status :: Lens.Lens' StatusReport (Prelude.Maybe Prelude.Text)
statusReport_status = Lens.lens (\StatusReport' {status} -> status) (\s@StatusReport' {} a -> s {status = a} :: StatusReport)

instance Data.FromXML StatusReport where
  parseXML x =
    StatusReport'
      Prelude.<$> (x Data..@? "CheckedTime")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable StatusReport where
  hashWithSalt _salt StatusReport' {..} =
    _salt
      `Prelude.hashWithSalt` checkedTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData StatusReport where
  rnf StatusReport' {..} =
    Prelude.rnf checkedTime
      `Prelude.seq` Prelude.rnf status
