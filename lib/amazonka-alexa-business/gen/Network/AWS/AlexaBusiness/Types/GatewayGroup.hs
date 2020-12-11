-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewayGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewayGroup
  ( GatewayGroup (..),

    -- * Smart constructor
    mkGatewayGroup,

    -- * Lenses
    ggARN,
    ggName,
    ggDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the gateway group.
--
-- /See:/ 'mkGatewayGroup' smart constructor.
data GatewayGroup = GatewayGroup'
  { arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GatewayGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the gateway group.
-- * 'description' - The description of the gateway group.
-- * 'name' - The name of the gateway group.
mkGatewayGroup ::
  GatewayGroup
mkGatewayGroup =
  GatewayGroup'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the gateway group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggARN :: Lens.Lens' GatewayGroup (Lude.Maybe Lude.Text)
ggARN = Lens.lens (arn :: GatewayGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GatewayGroup)
{-# DEPRECATED ggARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggName :: Lens.Lens' GatewayGroup (Lude.Maybe Lude.Text)
ggName = Lens.lens (name :: GatewayGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GatewayGroup)
{-# DEPRECATED ggName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggDescription :: Lens.Lens' GatewayGroup (Lude.Maybe Lude.Text)
ggDescription = Lens.lens (description :: GatewayGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GatewayGroup)
{-# DEPRECATED ggDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON GatewayGroup where
  parseJSON =
    Lude.withObject
      "GatewayGroup"
      ( \x ->
          GatewayGroup'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
      )
