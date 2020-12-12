{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AvailabilityZone
  ( AvailabilityZone (..),

    -- * Smart constructor
    mkAvailabilityZone,

    -- * Lenses
    azName,
    azSupportedPlatforms,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SupportedPlatform

-- | Describes an availability zone.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { name ::
      Lude.Maybe Lude.Text,
    supportedPlatforms :: Lude.Maybe [SupportedPlatform]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- * 'name' - The name of the availability zone.
-- * 'supportedPlatforms' -
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone =
  AvailabilityZone'
    { name = Lude.Nothing,
      supportedPlatforms = Lude.Nothing
    }

-- | The name of the availability zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azName :: Lens.Lens' AvailabilityZone (Lude.Maybe Lude.Text)
azName = Lens.lens (name :: AvailabilityZone -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AvailabilityZone)
{-# DEPRECATED azName "Use generic-lens or generic-optics with 'name' instead." #-}

-- |
--
-- /Note:/ Consider using 'supportedPlatforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azSupportedPlatforms :: Lens.Lens' AvailabilityZone (Lude.Maybe [SupportedPlatform])
azSupportedPlatforms = Lens.lens (supportedPlatforms :: AvailabilityZone -> Lude.Maybe [SupportedPlatform]) (\s a -> s {supportedPlatforms = a} :: AvailabilityZone)
{-# DEPRECATED azSupportedPlatforms "Use generic-lens or generic-optics with 'supportedPlatforms' instead." #-}

instance Lude.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone'
      Lude.<$> (x Lude..@? "Name")
      Lude.<*> ( x Lude..@? "SupportedPlatforms" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "SupportedPlatform")
               )
