-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CollectionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CollectionConfiguration
  ( CollectionConfiguration (..),

    -- * Smart constructor
    mkCollectionConfiguration,

    -- * Lenses
    ccCollectionParameters,
    ccCollectionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for tensor collections.
--
-- /See:/ 'mkCollectionConfiguration' smart constructor.
data CollectionConfiguration = CollectionConfiguration'
  { collectionParameters ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    collectionName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CollectionConfiguration' with the minimum fields required to make a request.
--
-- * 'collectionName' - The name of the tensor collection. The name must be unique relative to other rule configuration names.
-- * 'collectionParameters' - Parameter values for the tensor collection. The allowed parameters are @"name"@ , @"include_regex"@ , @"reduction_config"@ , @"save_config"@ , @"tensor_names"@ , and @"save_histogram"@ .
mkCollectionConfiguration ::
  CollectionConfiguration
mkCollectionConfiguration =
  CollectionConfiguration'
    { collectionParameters = Lude.Nothing,
      collectionName = Lude.Nothing
    }

-- | Parameter values for the tensor collection. The allowed parameters are @"name"@ , @"include_regex"@ , @"reduction_config"@ , @"save_config"@ , @"tensor_names"@ , and @"save_histogram"@ .
--
-- /Note:/ Consider using 'collectionParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCollectionParameters :: Lens.Lens' CollectionConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccCollectionParameters = Lens.lens (collectionParameters :: CollectionConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {collectionParameters = a} :: CollectionConfiguration)
{-# DEPRECATED ccCollectionParameters "Use generic-lens or generic-optics with 'collectionParameters' instead." #-}

-- | The name of the tensor collection. The name must be unique relative to other rule configuration names.
--
-- /Note:/ Consider using 'collectionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCollectionName :: Lens.Lens' CollectionConfiguration (Lude.Maybe Lude.Text)
ccCollectionName = Lens.lens (collectionName :: CollectionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {collectionName = a} :: CollectionConfiguration)
{-# DEPRECATED ccCollectionName "Use generic-lens or generic-optics with 'collectionName' instead." #-}

instance Lude.FromJSON CollectionConfiguration where
  parseJSON =
    Lude.withObject
      "CollectionConfiguration"
      ( \x ->
          CollectionConfiguration'
            Lude.<$> (x Lude..:? "CollectionParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CollectionName")
      )

instance Lude.ToJSON CollectionConfiguration where
  toJSON CollectionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CollectionParameters" Lude..=) Lude.<$> collectionParameters,
            ("CollectionName" Lude..=) Lude.<$> collectionName
          ]
      )
