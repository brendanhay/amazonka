{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Protection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Protection
  ( Protection (..),

    -- * Smart constructor
    mkProtection,

    -- * Lenses
    pHealthCheckIds,
    pResourceARN,
    pName,
    pId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents a resource that is under DDoS protection.
--
-- /See:/ 'mkProtection' smart constructor.
data Protection = Protection'
  { -- | The unique identifier (ID) for the Route 53 health check that's associated with the protection.
    healthCheckIds :: Lude.Maybe [Lude.Text],
    -- | The ARN (Amazon Resource Name) of the AWS resource that is protected.
    resourceARN :: Lude.Maybe Lude.Text,
    -- | The name of the protection. For example, @My CloudFront distributions@ .
    name :: Lude.Maybe Lude.Text,
    -- | The unique identifier (ID) of the protection.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Protection' with the minimum fields required to make a request.
--
-- * 'healthCheckIds' - The unique identifier (ID) for the Route 53 health check that's associated with the protection.
-- * 'resourceARN' - The ARN (Amazon Resource Name) of the AWS resource that is protected.
-- * 'name' - The name of the protection. For example, @My CloudFront distributions@ .
-- * 'id' - The unique identifier (ID) of the protection.
mkProtection ::
  Protection
mkProtection =
  Protection'
    { healthCheckIds = Lude.Nothing,
      resourceARN = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The unique identifier (ID) for the Route 53 health check that's associated with the protection.
--
-- /Note:/ Consider using 'healthCheckIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pHealthCheckIds :: Lens.Lens' Protection (Lude.Maybe [Lude.Text])
pHealthCheckIds = Lens.lens (healthCheckIds :: Protection -> Lude.Maybe [Lude.Text]) (\s a -> s {healthCheckIds = a} :: Protection)
{-# DEPRECATED pHealthCheckIds "Use generic-lens or generic-optics with 'healthCheckIds' instead." #-}

-- | The ARN (Amazon Resource Name) of the AWS resource that is protected.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResourceARN :: Lens.Lens' Protection (Lude.Maybe Lude.Text)
pResourceARN = Lens.lens (resourceARN :: Protection -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: Protection)
{-# DEPRECATED pResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The name of the protection. For example, @My CloudFront distributions@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Protection (Lude.Maybe Lude.Text)
pName = Lens.lens (name :: Protection -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Protection)
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier (ID) of the protection.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pId :: Lens.Lens' Protection (Lude.Maybe Lude.Text)
pId = Lens.lens (id :: Protection -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Protection)
{-# DEPRECATED pId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON Protection where
  parseJSON =
    Lude.withObject
      "Protection"
      ( \x ->
          Protection'
            Lude.<$> (x Lude..:? "HealthCheckIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResourceArn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
