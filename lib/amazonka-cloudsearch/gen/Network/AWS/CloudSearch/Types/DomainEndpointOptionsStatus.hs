{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainEndpointOptionsStatus
  ( DomainEndpointOptionsStatus (..),

    -- * Smart constructor
    mkDomainEndpointOptionsStatus,

    -- * Lenses
    deosStatus,
    deosOptions,
  )
where

import Network.AWS.CloudSearch.Types.DomainEndpointOptions
import Network.AWS.CloudSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration and status of the domain's endpoint options.
--
-- /See:/ 'mkDomainEndpointOptionsStatus' smart constructor.
data DomainEndpointOptionsStatus = DomainEndpointOptionsStatus'
  { -- | The status of the configured domain endpoint options.
    status :: OptionStatus,
    -- | The domain endpoint options configured for the domain.
    options :: DomainEndpointOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainEndpointOptionsStatus' with the minimum fields required to make a request.
--
-- * 'status' - The status of the configured domain endpoint options.
-- * 'options' - The domain endpoint options configured for the domain.
mkDomainEndpointOptionsStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  DomainEndpointOptions ->
  DomainEndpointOptionsStatus
mkDomainEndpointOptionsStatus pStatus_ pOptions_ =
  DomainEndpointOptionsStatus'
    { status = pStatus_,
      options = pOptions_
    }

-- | The status of the configured domain endpoint options.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deosStatus :: Lens.Lens' DomainEndpointOptionsStatus OptionStatus
deosStatus = Lens.lens (status :: DomainEndpointOptionsStatus -> OptionStatus) (\s a -> s {status = a} :: DomainEndpointOptionsStatus)
{-# DEPRECATED deosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The domain endpoint options configured for the domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deosOptions :: Lens.Lens' DomainEndpointOptionsStatus DomainEndpointOptions
deosOptions = Lens.lens (options :: DomainEndpointOptionsStatus -> DomainEndpointOptions) (\s a -> s {options = a} :: DomainEndpointOptionsStatus)
{-# DEPRECATED deosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromXML DomainEndpointOptionsStatus where
  parseXML x =
    DomainEndpointOptionsStatus'
      Lude.<$> (x Lude..@ "Status") Lude.<*> (x Lude..@ "Options")
