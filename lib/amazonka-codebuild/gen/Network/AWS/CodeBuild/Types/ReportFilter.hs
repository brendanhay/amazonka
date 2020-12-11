-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportFilter
  ( ReportFilter (..),

    -- * Smart constructor
    mkReportFilter,

    -- * Lenses
    rfStatus,
  )
where

import Network.AWS.CodeBuild.Types.ReportStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A filter used to return reports with the status specified by the input @status@ parameter.
--
-- /See:/ 'mkReportFilter' smart constructor.
newtype ReportFilter = ReportFilter'
  { status ::
      Lude.Maybe ReportStatusType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportFilter' with the minimum fields required to make a request.
--
-- * 'status' - The status used to filter reports. You can filter using one status only.
mkReportFilter ::
  ReportFilter
mkReportFilter = ReportFilter' {status = Lude.Nothing}

-- | The status used to filter reports. You can filter using one status only.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfStatus :: Lens.Lens' ReportFilter (Lude.Maybe ReportStatusType)
rfStatus = Lens.lens (status :: ReportFilter -> Lude.Maybe ReportStatusType) (\s a -> s {status = a} :: ReportFilter)
{-# DEPRECATED rfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.ToJSON ReportFilter where
  toJSON ReportFilter' {..} =
    Lude.object (Lude.catMaybes [("status" Lude..=) Lude.<$> status])
