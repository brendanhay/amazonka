{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.S3KeyFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.S3KeyFilter
  ( S3KeyFilter (..),

    -- * Smart constructor
    mkS3KeyFilter,

    -- * Lenses
    skfFilterRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.FilterRule

-- | A container for object key name prefix and suffix filtering rules.
--
-- /See:/ 'mkS3KeyFilter' smart constructor.
newtype S3KeyFilter = S3KeyFilter'
  { filterRules :: Lude.Maybe [FilterRule]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3KeyFilter' with the minimum fields required to make a request.
--
-- * 'filterRules' -
mkS3KeyFilter ::
  S3KeyFilter
mkS3KeyFilter = S3KeyFilter' {filterRules = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filterRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skfFilterRules :: Lens.Lens' S3KeyFilter (Lude.Maybe [FilterRule])
skfFilterRules = Lens.lens (filterRules :: S3KeyFilter -> Lude.Maybe [FilterRule]) (\s a -> s {filterRules = a} :: S3KeyFilter)
{-# DEPRECATED skfFilterRules "Use generic-lens or generic-optics with 'filterRules' instead." #-}

instance Lude.FromXML S3KeyFilter where
  parseXML x =
    S3KeyFilter'
      Lude.<$> (Lude.may (Lude.parseXMLList "FilterRule") x)

instance Lude.ToXML S3KeyFilter where
  toXML S3KeyFilter' {..} =
    Lude.mconcat
      [Lude.toXML (Lude.toXMLList "FilterRule" Lude.<$> filterRules)]
