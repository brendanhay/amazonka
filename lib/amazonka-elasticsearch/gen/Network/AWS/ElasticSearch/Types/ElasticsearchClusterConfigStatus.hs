{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
  ( ElasticsearchClusterConfigStatus (..),

    -- * Smart constructor
    mkElasticsearchClusterConfigStatus,

    -- * Lenses
    eccsStatus,
    eccsOptions,
  )
where

import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration status for the specified Elasticsearch domain.
--
-- /See:/ 'mkElasticsearchClusterConfigStatus' smart constructor.
data ElasticsearchClusterConfigStatus = ElasticsearchClusterConfigStatus'
  { -- | Specifies the status of the configuration for the specified Elasticsearch domain.
    status :: OptionStatus,
    -- | Specifies the cluster configuration for the specified Elasticsearch domain.
    options :: ElasticsearchClusterConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchClusterConfigStatus' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the status of the configuration for the specified Elasticsearch domain.
-- * 'options' - Specifies the cluster configuration for the specified Elasticsearch domain.
mkElasticsearchClusterConfigStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  ElasticsearchClusterConfig ->
  ElasticsearchClusterConfigStatus
mkElasticsearchClusterConfigStatus pStatus_ pOptions_ =
  ElasticsearchClusterConfigStatus'
    { status = pStatus_,
      options = pOptions_
    }

-- | Specifies the status of the configuration for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccsStatus :: Lens.Lens' ElasticsearchClusterConfigStatus OptionStatus
eccsStatus = Lens.lens (status :: ElasticsearchClusterConfigStatus -> OptionStatus) (\s a -> s {status = a} :: ElasticsearchClusterConfigStatus)
{-# DEPRECATED eccsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the cluster configuration for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eccsOptions :: Lens.Lens' ElasticsearchClusterConfigStatus ElasticsearchClusterConfig
eccsOptions = Lens.lens (options :: ElasticsearchClusterConfigStatus -> ElasticsearchClusterConfig) (\s a -> s {options = a} :: ElasticsearchClusterConfigStatus)
{-# DEPRECATED eccsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromJSON ElasticsearchClusterConfigStatus where
  parseJSON =
    Lude.withObject
      "ElasticsearchClusterConfigStatus"
      ( \x ->
          ElasticsearchClusterConfigStatus'
            Lude.<$> (x Lude..: "Status") Lude.<*> (x Lude..: "Options")
      )
