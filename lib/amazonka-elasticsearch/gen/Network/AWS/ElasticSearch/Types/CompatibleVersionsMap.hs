{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
  ( CompatibleVersionsMap (..),

    -- * Smart constructor
    mkCompatibleVersionsMap,

    -- * Lenses
    cvmSourceVersion,
    cvmTargetVersions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A map from an @'ElasticsearchVersion' @ to a list of compatible @'ElasticsearchVersion' @ s to which the domain can be upgraded.
--
-- /See:/ 'mkCompatibleVersionsMap' smart constructor.
data CompatibleVersionsMap = CompatibleVersionsMap'
  { sourceVersion ::
      Lude.Maybe Lude.Text,
    targetVersions :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompatibleVersionsMap' with the minimum fields required to make a request.
--
-- * 'sourceVersion' - The current version of Elasticsearch on which a domain is.
-- * 'targetVersions' - Undocumented field.
mkCompatibleVersionsMap ::
  CompatibleVersionsMap
mkCompatibleVersionsMap =
  CompatibleVersionsMap'
    { sourceVersion = Lude.Nothing,
      targetVersions = Lude.Nothing
    }

-- | The current version of Elasticsearch on which a domain is.
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmSourceVersion :: Lens.Lens' CompatibleVersionsMap (Lude.Maybe Lude.Text)
cvmSourceVersion = Lens.lens (sourceVersion :: CompatibleVersionsMap -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: CompatibleVersionsMap)
{-# DEPRECATED cvmSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'targetVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvmTargetVersions :: Lens.Lens' CompatibleVersionsMap (Lude.Maybe [Lude.Text])
cvmTargetVersions = Lens.lens (targetVersions :: CompatibleVersionsMap -> Lude.Maybe [Lude.Text]) (\s a -> s {targetVersions = a} :: CompatibleVersionsMap)
{-# DEPRECATED cvmTargetVersions "Use generic-lens or generic-optics with 'targetVersions' instead." #-}

instance Lude.FromJSON CompatibleVersionsMap where
  parseJSON =
    Lude.withObject
      "CompatibleVersionsMap"
      ( \x ->
          CompatibleVersionsMap'
            Lude.<$> (x Lude..:? "SourceVersion")
            Lude.<*> (x Lude..:? "TargetVersions" Lude..!= Lude.mempty)
      )
