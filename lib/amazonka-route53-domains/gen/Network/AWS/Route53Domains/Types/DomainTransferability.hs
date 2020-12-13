{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainTransferability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainTransferability
  ( DomainTransferability (..),

    -- * Smart constructor
    mkDomainTransferability,

    -- * Lenses
    dtTransferable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53Domains.Types.Transferable

-- | A complex type that contains information about whether the specified domain can be transferred to Route 53.
--
-- /See:/ 'mkDomainTransferability' smart constructor.
newtype DomainTransferability = DomainTransferability'
  { transferable :: Lude.Maybe Transferable
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainTransferability' with the minimum fields required to make a request.
--
-- * 'transferable' -
mkDomainTransferability ::
  DomainTransferability
mkDomainTransferability =
  DomainTransferability' {transferable = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'transferable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTransferable :: Lens.Lens' DomainTransferability (Lude.Maybe Transferable)
dtTransferable = Lens.lens (transferable :: DomainTransferability -> Lude.Maybe Transferable) (\s a -> s {transferable = a} :: DomainTransferability)
{-# DEPRECATED dtTransferable "Use generic-lens or generic-optics with 'transferable' instead." #-}

instance Lude.FromJSON DomainTransferability where
  parseJSON =
    Lude.withObject
      "DomainTransferability"
      ( \x ->
          DomainTransferability' Lude.<$> (x Lude..:? "Transferable")
      )
