{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionSummary
  ( ServiceActionSummary (..),

    -- * Smart constructor
    mkServiceActionSummary,

    -- * Lenses
    sasName,
    sasId,
    sasDefinitionType,
    sasDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType

-- | Detailed information about the self-service action.
--
-- /See:/ 'mkServiceActionSummary' smart constructor.
data ServiceActionSummary = ServiceActionSummary'
  { -- | The self-service action name.
    name :: Lude.Maybe Lude.Text,
    -- | The self-service action identifier.
    id :: Lude.Maybe Lude.Text,
    -- | The self-service action definition type. For example, @SSM_AUTOMATION@ .
    definitionType :: Lude.Maybe ServiceActionDefinitionType,
    -- | The self-service action description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceActionSummary' with the minimum fields required to make a request.
--
-- * 'name' - The self-service action name.
-- * 'id' - The self-service action identifier.
-- * 'definitionType' - The self-service action definition type. For example, @SSM_AUTOMATION@ .
-- * 'description' - The self-service action description.
mkServiceActionSummary ::
  ServiceActionSummary
mkServiceActionSummary =
  ServiceActionSummary'
    { name = Lude.Nothing,
      id = Lude.Nothing,
      definitionType = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The self-service action name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasName :: Lens.Lens' ServiceActionSummary (Lude.Maybe Lude.Text)
sasName = Lens.lens (name :: ServiceActionSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ServiceActionSummary)
{-# DEPRECATED sasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The self-service action identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasId :: Lens.Lens' ServiceActionSummary (Lude.Maybe Lude.Text)
sasId = Lens.lens (id :: ServiceActionSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ServiceActionSummary)
{-# DEPRECATED sasId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The self-service action definition type. For example, @SSM_AUTOMATION@ .
--
-- /Note:/ Consider using 'definitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasDefinitionType :: Lens.Lens' ServiceActionSummary (Lude.Maybe ServiceActionDefinitionType)
sasDefinitionType = Lens.lens (definitionType :: ServiceActionSummary -> Lude.Maybe ServiceActionDefinitionType) (\s a -> s {definitionType = a} :: ServiceActionSummary)
{-# DEPRECATED sasDefinitionType "Use generic-lens or generic-optics with 'definitionType' instead." #-}

-- | The self-service action description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasDescription :: Lens.Lens' ServiceActionSummary (Lude.Maybe Lude.Text)
sasDescription = Lens.lens (description :: ServiceActionSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ServiceActionSummary)
{-# DEPRECATED sasDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ServiceActionSummary where
  parseJSON =
    Lude.withObject
      "ServiceActionSummary"
      ( \x ->
          ServiceActionSummary'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "DefinitionType")
            Lude.<*> (x Lude..:? "Description")
      )
