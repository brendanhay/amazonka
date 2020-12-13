{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
  ( ElasticsearchVersionStatus (..),

    -- * Smart constructor
    mkElasticsearchVersionStatus,

    -- * Lenses
    evsStatus,
    evsOptions,
  )
where

import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status of the Elasticsearch version options for the specified Elasticsearch domain.
--
-- /See:/ 'mkElasticsearchVersionStatus' smart constructor.
data ElasticsearchVersionStatus = ElasticsearchVersionStatus'
  { -- | Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
    status :: OptionStatus,
    -- | Specifies the Elasticsearch version for the specified Elasticsearch domain.
    options :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticsearchVersionStatus' with the minimum fields required to make a request.
--
-- * 'status' - Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
-- * 'options' - Specifies the Elasticsearch version for the specified Elasticsearch domain.
mkElasticsearchVersionStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  Lude.Text ->
  ElasticsearchVersionStatus
mkElasticsearchVersionStatus pStatus_ pOptions_ =
  ElasticsearchVersionStatus'
    { status = pStatus_,
      options = pOptions_
    }

-- | Specifies the status of the Elasticsearch version options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evsStatus :: Lens.Lens' ElasticsearchVersionStatus OptionStatus
evsStatus = Lens.lens (status :: ElasticsearchVersionStatus -> OptionStatus) (\s a -> s {status = a} :: ElasticsearchVersionStatus)
{-# DEPRECATED evsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies the Elasticsearch version for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evsOptions :: Lens.Lens' ElasticsearchVersionStatus Lude.Text
evsOptions = Lens.lens (options :: ElasticsearchVersionStatus -> Lude.Text) (\s a -> s {options = a} :: ElasticsearchVersionStatus)
{-# DEPRECATED evsOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromJSON ElasticsearchVersionStatus where
  parseJSON =
    Lude.withObject
      "ElasticsearchVersionStatus"
      ( \x ->
          ElasticsearchVersionStatus'
            Lude.<$> (x Lude..: "Status") Lude.<*> (x Lude..: "Options")
      )
