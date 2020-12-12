{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsConfiguration
  ( AnalyticsConfiguration (..),

    -- * Smart constructor
    mkAnalyticsConfiguration,

    -- * Lenses
    acFilter,
    acId,
    acStorageClassAnalysis,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsFilter
import Network.AWS.S3.Types.StorageClassAnalysis

-- | Specifies the configuration and any analyses for the analytics filter of an Amazon S3 bucket.
--
-- /See:/ 'mkAnalyticsConfiguration' smart constructor.
data AnalyticsConfiguration = AnalyticsConfiguration'
  { filter ::
      Lude.Maybe AnalyticsFilter,
    id :: Lude.Text,
    storageClassAnalysis :: StorageClassAnalysis
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AnalyticsConfiguration' with the minimum fields required to make a request.
--
-- * 'filter' - The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
-- * 'id' - The ID that identifies the analytics configuration.
-- * 'storageClassAnalysis' - Contains data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes.
mkAnalyticsConfiguration ::
  -- | 'id'
  Lude.Text ->
  -- | 'storageClassAnalysis'
  StorageClassAnalysis ->
  AnalyticsConfiguration
mkAnalyticsConfiguration pId_ pStorageClassAnalysis_ =
  AnalyticsConfiguration'
    { filter = Lude.Nothing,
      id = pId_,
      storageClassAnalysis = pStorageClassAnalysis_
    }

-- | The filter used to describe a set of objects for analyses. A filter must have exactly one prefix, one tag, or one conjunction (AnalyticsAndOperator). If no filter is provided, all objects will be considered in any analysis.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acFilter :: Lens.Lens' AnalyticsConfiguration (Lude.Maybe AnalyticsFilter)
acFilter = Lens.lens (filter :: AnalyticsConfiguration -> Lude.Maybe AnalyticsFilter) (\s a -> s {filter = a} :: AnalyticsConfiguration)
{-# DEPRECATED acFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The ID that identifies the analytics configuration.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acId :: Lens.Lens' AnalyticsConfiguration Lude.Text
acId = Lens.lens (id :: AnalyticsConfiguration -> Lude.Text) (\s a -> s {id = a} :: AnalyticsConfiguration)
{-# DEPRECATED acId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Contains data related to access patterns to be collected and made available to analyze the tradeoffs between different storage classes.
--
-- /Note:/ Consider using 'storageClassAnalysis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acStorageClassAnalysis :: Lens.Lens' AnalyticsConfiguration StorageClassAnalysis
acStorageClassAnalysis = Lens.lens (storageClassAnalysis :: AnalyticsConfiguration -> StorageClassAnalysis) (\s a -> s {storageClassAnalysis = a} :: AnalyticsConfiguration)
{-# DEPRECATED acStorageClassAnalysis "Use generic-lens or generic-optics with 'storageClassAnalysis' instead." #-}

instance Lude.FromXML AnalyticsConfiguration where
  parseXML x =
    AnalyticsConfiguration'
      Lude.<$> (x Lude..@? "Filter")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "StorageClassAnalysis")

instance Lude.ToXML AnalyticsConfiguration where
  toXML AnalyticsConfiguration' {..} =
    Lude.mconcat
      [ "Filter" Lude.@= filter,
        "Id" Lude.@= id,
        "StorageClassAnalysis" Lude.@= storageClassAnalysis
      ]
