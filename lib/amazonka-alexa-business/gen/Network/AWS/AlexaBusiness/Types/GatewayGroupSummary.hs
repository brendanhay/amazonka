-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
  ( GatewayGroupSummary (..),

    -- * Smart constructor
    mkGatewayGroupSummary,

    -- * Lenses
    ggsARN,
    ggsName,
    ggsDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The summary of a gateway group.
--
-- /See:/ 'mkGatewayGroupSummary' smart constructor.
data GatewayGroupSummary = GatewayGroupSummary'
  { arn ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GatewayGroupSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the gateway group.
-- * 'description' - The description of the gateway group.
-- * 'name' - The name of the gateway group.
mkGatewayGroupSummary ::
  GatewayGroupSummary
mkGatewayGroupSummary =
  GatewayGroupSummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the gateway group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggsARN :: Lens.Lens' GatewayGroupSummary (Lude.Maybe Lude.Text)
ggsARN = Lens.lens (arn :: GatewayGroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GatewayGroupSummary)
{-# DEPRECATED ggsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggsName :: Lens.Lens' GatewayGroupSummary (Lude.Maybe Lude.Text)
ggsName = Lens.lens (name :: GatewayGroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GatewayGroupSummary)
{-# DEPRECATED ggsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggsDescription :: Lens.Lens' GatewayGroupSummary (Lude.Maybe Lude.Text)
ggsDescription = Lens.lens (description :: GatewayGroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GatewayGroupSummary)
{-# DEPRECATED ggsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON GatewayGroupSummary where
  parseJSON =
    Lude.withObject
      "GatewayGroupSummary"
      ( \x ->
          GatewayGroupSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
      )
