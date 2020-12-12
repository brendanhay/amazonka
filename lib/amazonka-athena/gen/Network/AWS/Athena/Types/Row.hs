{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Row
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Row
  ( Row (..),

    -- * Smart constructor
    mkRow,

    -- * Lenses
    rData,
  )
where

import Network.AWS.Athena.Types.Datum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The rows that comprise a query result table.
--
-- /See:/ 'mkRow' smart constructor.
newtype Row = Row' {data' :: Lude.Maybe [Datum]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Row' with the minimum fields required to make a request.
--
-- * 'data'' - The data that populates a row in a query result table.
mkRow ::
  Row
mkRow = Row' {data' = Lude.Nothing}

-- | The data that populates a row in a query result table.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rData :: Lens.Lens' Row (Lude.Maybe [Datum])
rData = Lens.lens (data' :: Row -> Lude.Maybe [Datum]) (\s a -> s {data' = a} :: Row)
{-# DEPRECATED rData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Lude.FromJSON Row where
  parseJSON =
    Lude.withObject
      "Row"
      (\x -> Row' Lude.<$> (x Lude..:? "Data" Lude..!= Lude.mempty))
