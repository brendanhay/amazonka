-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionDetail
  ( ServiceActionDetail (..),

    -- * Smart constructor
    mkServiceActionDetail,

    -- * Lenses
    sadServiceActionSummary,
    sadDefinition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
import Network.AWS.ServiceCatalog.Types.ServiceActionSummary

-- | An object containing detailed information about the self-service action.
--
-- /See:/ 'mkServiceActionDetail' smart constructor.
data ServiceActionDetail = ServiceActionDetail'
  { serviceActionSummary ::
      Lude.Maybe ServiceActionSummary,
    definition ::
      Lude.Maybe
        ( Lude.HashMap
            ServiceActionDefinitionKey
            (Lude.Text)
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceActionDetail' with the minimum fields required to make a request.
--
-- * 'definition' - A map that defines the self-service action.
-- * 'serviceActionSummary' - Summary information about the self-service action.
mkServiceActionDetail ::
  ServiceActionDetail
mkServiceActionDetail =
  ServiceActionDetail'
    { serviceActionSummary = Lude.Nothing,
      definition = Lude.Nothing
    }

-- | Summary information about the self-service action.
--
-- /Note:/ Consider using 'serviceActionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadServiceActionSummary :: Lens.Lens' ServiceActionDetail (Lude.Maybe ServiceActionSummary)
sadServiceActionSummary = Lens.lens (serviceActionSummary :: ServiceActionDetail -> Lude.Maybe ServiceActionSummary) (\s a -> s {serviceActionSummary = a} :: ServiceActionDetail)
{-# DEPRECATED sadServiceActionSummary "Use generic-lens or generic-optics with 'serviceActionSummary' instead." #-}

-- | A map that defines the self-service action.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sadDefinition :: Lens.Lens' ServiceActionDetail (Lude.Maybe (Lude.HashMap ServiceActionDefinitionKey (Lude.Text)))
sadDefinition = Lens.lens (definition :: ServiceActionDetail -> Lude.Maybe (Lude.HashMap ServiceActionDefinitionKey (Lude.Text))) (\s a -> s {definition = a} :: ServiceActionDetail)
{-# DEPRECATED sadDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

instance Lude.FromJSON ServiceActionDetail where
  parseJSON =
    Lude.withObject
      "ServiceActionDetail"
      ( \x ->
          ServiceActionDetail'
            Lude.<$> (x Lude..:? "ServiceActionSummary")
            Lude.<*> (x Lude..:? "Definition" Lude..!= Lude.mempty)
      )
