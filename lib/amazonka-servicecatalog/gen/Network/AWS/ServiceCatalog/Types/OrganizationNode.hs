{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.OrganizationNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.OrganizationNode
  ( OrganizationNode (..),

    -- * Smart constructor
    mkOrganizationNode,

    -- * Lenses
    onValue,
    onType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.OrganizationNodeType

-- | Information about the organization node.
--
-- /See:/ 'mkOrganizationNode' smart constructor.
data OrganizationNode = OrganizationNode'
  { -- | The identifier of the organization node.
    value :: Lude.Maybe Lude.Text,
    -- | The organization node type.
    type' :: Lude.Maybe OrganizationNodeType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationNode' with the minimum fields required to make a request.
--
-- * 'value' - The identifier of the organization node.
-- * 'type'' - The organization node type.
mkOrganizationNode ::
  OrganizationNode
mkOrganizationNode =
  OrganizationNode' {value = Lude.Nothing, type' = Lude.Nothing}

-- | The identifier of the organization node.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
onValue :: Lens.Lens' OrganizationNode (Lude.Maybe Lude.Text)
onValue = Lens.lens (value :: OrganizationNode -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: OrganizationNode)
{-# DEPRECATED onValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The organization node type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
onType :: Lens.Lens' OrganizationNode (Lude.Maybe OrganizationNodeType)
onType = Lens.lens (type' :: OrganizationNode -> Lude.Maybe OrganizationNodeType) (\s a -> s {type' = a} :: OrganizationNode)
{-# DEPRECATED onType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON OrganizationNode where
  parseJSON =
    Lude.withObject
      "OrganizationNode"
      ( \x ->
          OrganizationNode'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON OrganizationNode where
  toJSON OrganizationNode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Value" Lude..=) Lude.<$> value,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )
