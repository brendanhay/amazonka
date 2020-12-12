{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.SourceRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.SourceRegion
  ( SourceRegion (..),

    -- * Smart constructor
    mkSourceRegion,

    -- * Lenses
    srStatus,
    srRegionName,
    srEndpoint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains an AWS Region name as the result of a successful call to the @DescribeSourceRegions@ action.
--
-- /See:/ 'mkSourceRegion' smart constructor.
data SourceRegion = SourceRegion'
  { status :: Lude.Maybe Lude.Text,
    regionName :: Lude.Maybe Lude.Text,
    endpoint :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceRegion' with the minimum fields required to make a request.
--
-- * 'endpoint' - The endpoint for the source AWS Region endpoint.
-- * 'regionName' - The name of the source AWS Region.
-- * 'status' - The status of the source AWS Region.
mkSourceRegion ::
  SourceRegion
mkSourceRegion =
  SourceRegion'
    { status = Lude.Nothing,
      regionName = Lude.Nothing,
      endpoint = Lude.Nothing
    }

-- | The status of the source AWS Region.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStatus :: Lens.Lens' SourceRegion (Lude.Maybe Lude.Text)
srStatus = Lens.lens (status :: SourceRegion -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: SourceRegion)
{-# DEPRECATED srStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the source AWS Region.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRegionName :: Lens.Lens' SourceRegion (Lude.Maybe Lude.Text)
srRegionName = Lens.lens (regionName :: SourceRegion -> Lude.Maybe Lude.Text) (\s a -> s {regionName = a} :: SourceRegion)
{-# DEPRECATED srRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The endpoint for the source AWS Region endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srEndpoint :: Lens.Lens' SourceRegion (Lude.Maybe Lude.Text)
srEndpoint = Lens.lens (endpoint :: SourceRegion -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: SourceRegion)
{-# DEPRECATED srEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Lude.FromXML SourceRegion where
  parseXML x =
    SourceRegion'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "RegionName")
      Lude.<*> (x Lude..@? "Endpoint")
