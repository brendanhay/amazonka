{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AliasRoutingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AliasRoutingConfiguration
  ( AliasRoutingConfiguration (..),

    -- * Smart constructor
    mkAliasRoutingConfiguration,

    -- * Lenses
    arcAdditionalVersionWeights,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html traffic-shifting> configuration of a Lambda function alias.
--
-- /See:/ 'mkAliasRoutingConfiguration' smart constructor.
newtype AliasRoutingConfiguration = AliasRoutingConfiguration'
  { -- | The second version, and the percentage of traffic that's routed to it.
    additionalVersionWeights :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AliasRoutingConfiguration' with the minimum fields required to make a request.
--
-- * 'additionalVersionWeights' - The second version, and the percentage of traffic that's routed to it.
mkAliasRoutingConfiguration ::
  AliasRoutingConfiguration
mkAliasRoutingConfiguration =
  AliasRoutingConfiguration'
    { additionalVersionWeights =
        Lude.Nothing
    }

-- | The second version, and the percentage of traffic that's routed to it.
--
-- /Note:/ Consider using 'additionalVersionWeights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arcAdditionalVersionWeights :: Lens.Lens' AliasRoutingConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double)))
arcAdditionalVersionWeights = Lens.lens (additionalVersionWeights :: AliasRoutingConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Double))) (\s a -> s {additionalVersionWeights = a} :: AliasRoutingConfiguration)
{-# DEPRECATED arcAdditionalVersionWeights "Use generic-lens or generic-optics with 'additionalVersionWeights' instead." #-}

instance Lude.FromJSON AliasRoutingConfiguration where
  parseJSON =
    Lude.withObject
      "AliasRoutingConfiguration"
      ( \x ->
          AliasRoutingConfiguration'
            Lude.<$> (x Lude..:? "AdditionalVersionWeights" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AliasRoutingConfiguration where
  toJSON AliasRoutingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AdditionalVersionWeights" Lude..=)
              Lude.<$> additionalVersionWeights
          ]
      )
