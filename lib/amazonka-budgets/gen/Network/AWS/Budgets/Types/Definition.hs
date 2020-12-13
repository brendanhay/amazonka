{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Definition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Definition
  ( Definition (..),

    -- * Smart constructor
    mkDefinition,

    -- * Lenses
    dScpActionDefinition,
    dIAMActionDefinition,
    dSsmActionDefinition,
  )
where

import Network.AWS.Budgets.Types.IAMActionDefinition
import Network.AWS.Budgets.Types.ScpActionDefinition
import Network.AWS.Budgets.Types.SsmActionDefinition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies all of the type-specific parameters.
--
-- /See:/ 'mkDefinition' smart constructor.
data Definition = Definition'
  { -- | The service control policies (SCPs) action definition details.
    scpActionDefinition :: Lude.Maybe ScpActionDefinition,
    -- | The AWS Identity and Access Management (IAM) action definition details.
    iamActionDefinition :: Lude.Maybe IAMActionDefinition,
    -- | The AWS Systems Manager (SSM) action definition details.
    ssmActionDefinition :: Lude.Maybe SsmActionDefinition
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Definition' with the minimum fields required to make a request.
--
-- * 'scpActionDefinition' - The service control policies (SCPs) action definition details.
-- * 'iamActionDefinition' - The AWS Identity and Access Management (IAM) action definition details.
-- * 'ssmActionDefinition' - The AWS Systems Manager (SSM) action definition details.
mkDefinition ::
  Definition
mkDefinition =
  Definition'
    { scpActionDefinition = Lude.Nothing,
      iamActionDefinition = Lude.Nothing,
      ssmActionDefinition = Lude.Nothing
    }

-- | The service control policies (SCPs) action definition details.
--
-- /Note:/ Consider using 'scpActionDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScpActionDefinition :: Lens.Lens' Definition (Lude.Maybe ScpActionDefinition)
dScpActionDefinition = Lens.lens (scpActionDefinition :: Definition -> Lude.Maybe ScpActionDefinition) (\s a -> s {scpActionDefinition = a} :: Definition)
{-# DEPRECATED dScpActionDefinition "Use generic-lens or generic-optics with 'scpActionDefinition' instead." #-}

-- | The AWS Identity and Access Management (IAM) action definition details.
--
-- /Note:/ Consider using 'iamActionDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIAMActionDefinition :: Lens.Lens' Definition (Lude.Maybe IAMActionDefinition)
dIAMActionDefinition = Lens.lens (iamActionDefinition :: Definition -> Lude.Maybe IAMActionDefinition) (\s a -> s {iamActionDefinition = a} :: Definition)
{-# DEPRECATED dIAMActionDefinition "Use generic-lens or generic-optics with 'iamActionDefinition' instead." #-}

-- | The AWS Systems Manager (SSM) action definition details.
--
-- /Note:/ Consider using 'ssmActionDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSsmActionDefinition :: Lens.Lens' Definition (Lude.Maybe SsmActionDefinition)
dSsmActionDefinition = Lens.lens (ssmActionDefinition :: Definition -> Lude.Maybe SsmActionDefinition) (\s a -> s {ssmActionDefinition = a} :: Definition)
{-# DEPRECATED dSsmActionDefinition "Use generic-lens or generic-optics with 'ssmActionDefinition' instead." #-}

instance Lude.FromJSON Definition where
  parseJSON =
    Lude.withObject
      "Definition"
      ( \x ->
          Definition'
            Lude.<$> (x Lude..:? "ScpActionDefinition")
            Lude.<*> (x Lude..:? "IamActionDefinition")
            Lude.<*> (x Lude..:? "SsmActionDefinition")
      )

instance Lude.ToJSON Definition where
  toJSON Definition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ScpActionDefinition" Lude..=) Lude.<$> scpActionDefinition,
            ("IamActionDefinition" Lude..=) Lude.<$> iamActionDefinition,
            ("SsmActionDefinition" Lude..=) Lude.<$> ssmActionDefinition
          ]
      )
