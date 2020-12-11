-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClassAnalysis
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StorageClassAnalysis
  ( StorageClassAnalysis (..),

    -- * Smart constructor
    mkStorageClassAnalysis,

    -- * Lenses
    scaDataExport,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.StorageClassAnalysisDataExport

-- | Specifies data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes for an Amazon S3 bucket.
--
-- /See:/ 'mkStorageClassAnalysis' smart constructor.
newtype StorageClassAnalysis = StorageClassAnalysis'
  { dataExport ::
      Lude.Maybe StorageClassAnalysisDataExport
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StorageClassAnalysis' with the minimum fields required to make a request.
--
-- * 'dataExport' - Specifies how data related to the storage class analysis for an Amazon S3 bucket should be exported.
mkStorageClassAnalysis ::
  StorageClassAnalysis
mkStorageClassAnalysis =
  StorageClassAnalysis' {dataExport = Lude.Nothing}

-- | Specifies how data related to the storage class analysis for an Amazon S3 bucket should be exported.
--
-- /Note:/ Consider using 'dataExport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scaDataExport :: Lens.Lens' StorageClassAnalysis (Lude.Maybe StorageClassAnalysisDataExport)
scaDataExport = Lens.lens (dataExport :: StorageClassAnalysis -> Lude.Maybe StorageClassAnalysisDataExport) (\s a -> s {dataExport = a} :: StorageClassAnalysis)
{-# DEPRECATED scaDataExport "Use generic-lens or generic-optics with 'dataExport' instead." #-}

instance Lude.FromXML StorageClassAnalysis where
  parseXML x =
    StorageClassAnalysis' Lude.<$> (x Lude..@? "DataExport")

instance Lude.ToXML StorageClassAnalysis where
  toXML StorageClassAnalysis' {..} =
    Lude.mconcat ["DataExport" Lude.@= dataExport]
