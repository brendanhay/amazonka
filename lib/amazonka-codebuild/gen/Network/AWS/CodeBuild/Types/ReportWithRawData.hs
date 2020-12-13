{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportWithRawData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportWithRawData
  ( ReportWithRawData (..),

    -- * Smart constructor
    mkReportWithRawData,

    -- * Lenses
    rwrdData,
    rwrdReportARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkReportWithRawData' smart constructor.
data ReportWithRawData = ReportWithRawData'
  { data' :: Lude.Maybe Lude.Text,
    reportARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportWithRawData' with the minimum fields required to make a request.
--
-- * 'data'' -
-- * 'reportARN' -
mkReportWithRawData ::
  ReportWithRawData
mkReportWithRawData =
  ReportWithRawData'
    { data' = Lude.Nothing,
      reportARN = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrdData :: Lens.Lens' ReportWithRawData (Lude.Maybe Lude.Text)
rwrdData = Lens.lens (data' :: ReportWithRawData -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: ReportWithRawData)
{-# DEPRECATED rwrdData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrdReportARN :: Lens.Lens' ReportWithRawData (Lude.Maybe Lude.Text)
rwrdReportARN = Lens.lens (reportARN :: ReportWithRawData -> Lude.Maybe Lude.Text) (\s a -> s {reportARN = a} :: ReportWithRawData)
{-# DEPRECATED rwrdReportARN "Use generic-lens or generic-optics with 'reportARN' instead." #-}

instance Lude.FromJSON ReportWithRawData where
  parseJSON =
    Lude.withObject
      "ReportWithRawData"
      ( \x ->
          ReportWithRawData'
            Lude.<$> (x Lude..:? "data") Lude.<*> (x Lude..:? "reportArn")
      )
