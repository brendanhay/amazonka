{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
  ( ElasticsearchDataSourceConfig (..),

    -- * Smart constructor
    mkElasticsearchDataSourceConfig,

    -- * Lenses
    edscEndpoint,
    edscAwsRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Elasticsearch data source configuration.
--
-- /See:/ 'mkElasticsearchDataSourceConfig' smart constructor.
data ElasticsearchDataSourceConfig = ElasticsearchDataSourceConfig'
  { endpoint ::
      Lude.Text,
    awsRegion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchDataSourceConfig' with the minimum fields required to make a request.
--
-- * 'awsRegion' - The AWS Region.
-- * 'endpoint' - The endpoint.
mkElasticsearchDataSourceConfig ::
  -- | 'endpoint'
  Lude.Text ->
  -- | 'awsRegion'
  Lude.Text ->
  ElasticsearchDataSourceConfig
mkElasticsearchDataSourceConfig pEndpoint_ pAwsRegion_ =
  ElasticsearchDataSourceConfig'
    { endpoint = pEndpoint_,
      awsRegion = pAwsRegion_
    }

-- | The endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edscEndpoint :: Lens.Lens' ElasticsearchDataSourceConfig Lude.Text
edscEndpoint = Lens.lens (endpoint :: ElasticsearchDataSourceConfig -> Lude.Text) (\s a -> s {endpoint = a} :: ElasticsearchDataSourceConfig)
{-# DEPRECATED edscEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The AWS Region.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edscAwsRegion :: Lens.Lens' ElasticsearchDataSourceConfig Lude.Text
edscAwsRegion = Lens.lens (awsRegion :: ElasticsearchDataSourceConfig -> Lude.Text) (\s a -> s {awsRegion = a} :: ElasticsearchDataSourceConfig)
{-# DEPRECATED edscAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

instance Lude.FromJSON ElasticsearchDataSourceConfig where
  parseJSON =
    Lude.withObject
      "ElasticsearchDataSourceConfig"
      ( \x ->
          ElasticsearchDataSourceConfig'
            Lude.<$> (x Lude..: "endpoint") Lude.<*> (x Lude..: "awsRegion")
      )

instance Lude.ToJSON ElasticsearchDataSourceConfig where
  toJSON ElasticsearchDataSourceConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("endpoint" Lude..= endpoint),
            Lude.Just ("awsRegion" Lude..= awsRegion)
          ]
      )
