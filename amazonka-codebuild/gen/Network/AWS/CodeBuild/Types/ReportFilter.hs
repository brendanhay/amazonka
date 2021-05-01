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
-- Module      : Network.AWS.CodeBuild.Types.ReportFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportFilter where

import Network.AWS.CodeBuild.Types.ReportStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A filter used to return reports with the status specified by the input
-- @status@ parameter.
--
-- /See:/ 'newReportFilter' smart constructor.
data ReportFilter = ReportFilter'
  { -- | The status used to filter reports. You can filter using one status only.
    status :: Prelude.Maybe ReportStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReportFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'reportFilter_status' - The status used to filter reports. You can filter using one status only.
newReportFilter ::
  ReportFilter
newReportFilter =
  ReportFilter' {status = Prelude.Nothing}

-- | The status used to filter reports. You can filter using one status only.
reportFilter_status :: Lens.Lens' ReportFilter (Prelude.Maybe ReportStatusType)
reportFilter_status = Lens.lens (\ReportFilter' {status} -> status) (\s@ReportFilter' {} a -> s {status = a} :: ReportFilter)

instance Prelude.Hashable ReportFilter

instance Prelude.NFData ReportFilter

instance Prelude.ToJSON ReportFilter where
  toJSON ReportFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("status" Prelude..=) Prelude.<$> status]
      )
