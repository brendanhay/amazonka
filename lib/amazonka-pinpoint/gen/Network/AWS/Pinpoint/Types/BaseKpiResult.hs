{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaseKpiResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaseKpiResult
  ( BaseKpiResult (..),

    -- * Smart constructor
    mkBaseKpiResult,

    -- * Lenses
    bkrRows,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.ResultRow
import qualified Network.AWS.Prelude as Lude

-- | Provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
--
-- /See:/ 'mkBaseKpiResult' smart constructor.
newtype BaseKpiResult = BaseKpiResult'
  { -- | An array of objects that provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
    rows :: [ResultRow]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BaseKpiResult' with the minimum fields required to make a request.
--
-- * 'rows' - An array of objects that provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
mkBaseKpiResult ::
  BaseKpiResult
mkBaseKpiResult = BaseKpiResult' {rows = Lude.mempty}

-- | An array of objects that provides the results of a query that retrieved the data for a standard metric that applies to an application, campaign, or journey.
--
-- /Note:/ Consider using 'rows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bkrRows :: Lens.Lens' BaseKpiResult [ResultRow]
bkrRows = Lens.lens (rows :: BaseKpiResult -> [ResultRow]) (\s a -> s {rows = a} :: BaseKpiResult)
{-# DEPRECATED bkrRows "Use generic-lens or generic-optics with 'rows' instead." #-}

instance Lude.FromJSON BaseKpiResult where
  parseJSON =
    Lude.withObject
      "BaseKpiResult"
      ( \x ->
          BaseKpiResult' Lude.<$> (x Lude..:? "Rows" Lude..!= Lude.mempty)
      )
