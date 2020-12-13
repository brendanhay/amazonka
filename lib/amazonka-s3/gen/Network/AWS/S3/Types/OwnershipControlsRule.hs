{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OwnershipControlsRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OwnershipControlsRule
  ( OwnershipControlsRule (..),

    -- * Smart constructor
    mkOwnershipControlsRule,

    -- * Lenses
    ocrObjectOwnership,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectOwnership

-- | The container element for an ownership control rule.
--
-- /See:/ 'mkOwnershipControlsRule' smart constructor.
newtype OwnershipControlsRule = OwnershipControlsRule'
  { objectOwnership :: ObjectOwnership
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OwnershipControlsRule' with the minimum fields required to make a request.
--
-- * 'objectOwnership' -
mkOwnershipControlsRule ::
  -- | 'objectOwnership'
  ObjectOwnership ->
  OwnershipControlsRule
mkOwnershipControlsRule pObjectOwnership_ =
  OwnershipControlsRule' {objectOwnership = pObjectOwnership_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'objectOwnership' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrObjectOwnership :: Lens.Lens' OwnershipControlsRule ObjectOwnership
ocrObjectOwnership = Lens.lens (objectOwnership :: OwnershipControlsRule -> ObjectOwnership) (\s a -> s {objectOwnership = a} :: OwnershipControlsRule)
{-# DEPRECATED ocrObjectOwnership "Use generic-lens or generic-optics with 'objectOwnership' instead." #-}

instance Lude.FromXML OwnershipControlsRule where
  parseXML x =
    OwnershipControlsRule' Lude.<$> (x Lude..@ "ObjectOwnership")

instance Lude.ToXML OwnershipControlsRule where
  toXML OwnershipControlsRule' {..} =
    Lude.mconcat ["ObjectOwnership" Lude.@= objectOwnership]
