-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportSummary
  ( ExportSummary (..),

    -- * Smart constructor
    mkExportSummary,

    -- * Lenses
    esExportStatus,
    esExportARN,
  )
where

import Network.AWS.DynamoDB.Types.ExportStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about an export task.
--
-- /See:/ 'mkExportSummary' smart constructor.
data ExportSummary = ExportSummary'
  { exportStatus ::
      Lude.Maybe ExportStatus,
    exportARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportSummary' with the minimum fields required to make a request.
--
-- * 'exportARN' - The Amazon Resource Name (ARN) of the export.
-- * 'exportStatus' - Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
mkExportSummary ::
  ExportSummary
mkExportSummary =
  ExportSummary'
    { exportStatus = Lude.Nothing,
      exportARN = Lude.Nothing
    }

-- | Export can be in one of the following states: IN_PROGRESS, COMPLETED, or FAILED.
--
-- /Note:/ Consider using 'exportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExportStatus :: Lens.Lens' ExportSummary (Lude.Maybe ExportStatus)
esExportStatus = Lens.lens (exportStatus :: ExportSummary -> Lude.Maybe ExportStatus) (\s a -> s {exportStatus = a} :: ExportSummary)
{-# DEPRECATED esExportStatus "Use generic-lens or generic-optics with 'exportStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the export.
--
-- /Note:/ Consider using 'exportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esExportARN :: Lens.Lens' ExportSummary (Lude.Maybe Lude.Text)
esExportARN = Lens.lens (exportARN :: ExportSummary -> Lude.Maybe Lude.Text) (\s a -> s {exportARN = a} :: ExportSummary)
{-# DEPRECATED esExportARN "Use generic-lens or generic-optics with 'exportARN' instead." #-}

instance Lude.FromJSON ExportSummary where
  parseJSON =
    Lude.withObject
      "ExportSummary"
      ( \x ->
          ExportSummary'
            Lude.<$> (x Lude..:? "ExportStatus") Lude.<*> (x Lude..:? "ExportArn")
      )
