{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.QueryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.QueryInfo
  ( QueryInfo (..),

    -- * Smart constructor
    mkQueryInfo,

    -- * Lenses
    qiSelectFields,
  )
where

import Network.AWS.Config.Types.FieldInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the query.
--
-- /See:/ 'mkQueryInfo' smart constructor.
newtype QueryInfo = QueryInfo'
  { selectFields ::
      Lude.Maybe [FieldInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryInfo' with the minimum fields required to make a request.
--
-- * 'selectFields' - Returns a @FieldInfo@ object.
mkQueryInfo ::
  QueryInfo
mkQueryInfo = QueryInfo' {selectFields = Lude.Nothing}

-- | Returns a @FieldInfo@ object.
--
-- /Note:/ Consider using 'selectFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qiSelectFields :: Lens.Lens' QueryInfo (Lude.Maybe [FieldInfo])
qiSelectFields = Lens.lens (selectFields :: QueryInfo -> Lude.Maybe [FieldInfo]) (\s a -> s {selectFields = a} :: QueryInfo)
{-# DEPRECATED qiSelectFields "Use generic-lens or generic-optics with 'selectFields' instead." #-}

instance Lude.FromJSON QueryInfo where
  parseJSON =
    Lude.withObject
      "QueryInfo"
      ( \x ->
          QueryInfo'
            Lude.<$> (x Lude..:? "SelectFields" Lude..!= Lude.mempty)
      )
