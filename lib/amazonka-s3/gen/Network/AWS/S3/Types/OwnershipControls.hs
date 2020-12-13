{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OwnershipControls
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OwnershipControls
  ( OwnershipControls (..),

    -- * Smart constructor
    mkOwnershipControls,

    -- * Lenses
    ocRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.OwnershipControlsRule

-- | The container element for a bucket's ownership controls.
--
-- /See:/ 'mkOwnershipControls' smart constructor.
newtype OwnershipControls = OwnershipControls'
  { -- | The container element for an ownership control rule.
    rules :: [OwnershipControlsRule]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OwnershipControls' with the minimum fields required to make a request.
--
-- * 'rules' - The container element for an ownership control rule.
mkOwnershipControls ::
  OwnershipControls
mkOwnershipControls = OwnershipControls' {rules = Lude.mempty}

-- | The container element for an ownership control rule.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocRules :: Lens.Lens' OwnershipControls [OwnershipControlsRule]
ocRules = Lens.lens (rules :: OwnershipControls -> [OwnershipControlsRule]) (\s a -> s {rules = a} :: OwnershipControls)
{-# DEPRECATED ocRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Lude.FromXML OwnershipControls where
  parseXML x =
    OwnershipControls' Lude.<$> (Lude.parseXMLList "Rule" x)

instance Lude.ToXML OwnershipControls where
  toXML OwnershipControls' {..} =
    Lude.mconcat [Lude.toXMLList "Rule" rules]
