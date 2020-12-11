-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Query
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Query
  ( Query (..),

    -- * Smart constructor
    mkQuery,

    -- * Lenses
    qSelectors,
  )
where

import Network.AWS.DataPipeline.Types.Selector
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines the query to run against an object.
--
-- /See:/ 'mkQuery' smart constructor.
newtype Query = Query' {selectors :: Lude.Maybe [Selector]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Query' with the minimum fields required to make a request.
--
-- * 'selectors' - List of selectors that define the query. An object must satisfy all of the selectors to match the query.
mkQuery ::
  Query
mkQuery = Query' {selectors = Lude.Nothing}

-- | List of selectors that define the query. An object must satisfy all of the selectors to match the query.
--
-- /Note:/ Consider using 'selectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qSelectors :: Lens.Lens' Query (Lude.Maybe [Selector])
qSelectors = Lens.lens (selectors :: Query -> Lude.Maybe [Selector]) (\s a -> s {selectors = a} :: Query)
{-# DEPRECATED qSelectors "Use generic-lens or generic-optics with 'selectors' instead." #-}

instance Lude.ToJSON Query where
  toJSON Query' {..} =
    Lude.object
      (Lude.catMaybes [("selectors" Lude..=) Lude.<$> selectors])
