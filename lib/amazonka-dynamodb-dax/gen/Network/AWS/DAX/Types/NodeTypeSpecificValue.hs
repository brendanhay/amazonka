-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.NodeTypeSpecificValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.NodeTypeSpecificValue
  ( NodeTypeSpecificValue (..),

    -- * Smart constructor
    mkNodeTypeSpecificValue,

    -- * Lenses
    ntsvValue,
    ntsvNodeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a parameter value that is applicable to a particular node type.
--
-- /See:/ 'mkNodeTypeSpecificValue' smart constructor.
data NodeTypeSpecificValue = NodeTypeSpecificValue'
  { value ::
      Lude.Maybe Lude.Text,
    nodeType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeTypeSpecificValue' with the minimum fields required to make a request.
--
-- * 'nodeType' - A node type to which the parameter value applies.
-- * 'value' - The parameter value for this node type.
mkNodeTypeSpecificValue ::
  NodeTypeSpecificValue
mkNodeTypeSpecificValue =
  NodeTypeSpecificValue'
    { value = Lude.Nothing,
      nodeType = Lude.Nothing
    }

-- | The parameter value for this node type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntsvValue :: Lens.Lens' NodeTypeSpecificValue (Lude.Maybe Lude.Text)
ntsvValue = Lens.lens (value :: NodeTypeSpecificValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: NodeTypeSpecificValue)
{-# DEPRECATED ntsvValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | A node type to which the parameter value applies.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntsvNodeType :: Lens.Lens' NodeTypeSpecificValue (Lude.Maybe Lude.Text)
ntsvNodeType = Lens.lens (nodeType :: NodeTypeSpecificValue -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: NodeTypeSpecificValue)
{-# DEPRECATED ntsvNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

instance Lude.FromJSON NodeTypeSpecificValue where
  parseJSON =
    Lude.withObject
      "NodeTypeSpecificValue"
      ( \x ->
          NodeTypeSpecificValue'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "NodeType")
      )
