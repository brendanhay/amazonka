-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
  ( AdvancedOptionsStatus (..),

    -- * Smart constructor
    mkAdvancedOptionsStatus,

    -- * Lenses
    aosOptions,
    aosStatus,
  )
where

import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status of the advanced options for the specified Elasticsearch domain. Currently, the following advanced options are available:
--
--
--     * Option to allow references to indices in an HTTP request body. Must be @false@ when configuring access to individual sub-resources. By default, the value is @true@ . See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuration Advanced Options> for more information.
--
--     * Option to specify the percentage of heap space that is allocated to field data. By default, this setting is unbounded.
--
-- For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-advanced-options Configuring Advanced Options> .
--
-- /See:/ 'mkAdvancedOptionsStatus' smart constructor.
data AdvancedOptionsStatus = AdvancedOptionsStatus'
  { options ::
      Lude.HashMap Lude.Text (Lude.Text),
    status :: OptionStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdvancedOptionsStatus' with the minimum fields required to make a request.
--
-- * 'options' - Specifies the status of advanced options for the specified Elasticsearch domain.
-- * 'status' - Specifies the status of @OptionStatus@ for advanced options for the specified Elasticsearch domain.
mkAdvancedOptionsStatus ::
  -- | 'status'
  OptionStatus ->
  AdvancedOptionsStatus
mkAdvancedOptionsStatus pStatus_ =
  AdvancedOptionsStatus' {options = Lude.mempty, status = pStatus_}

-- | Specifies the status of advanced options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosOptions :: Lens.Lens' AdvancedOptionsStatus (Lude.HashMap Lude.Text (Lude.Text))
aosOptions = Lens.lens (options :: AdvancedOptionsStatus -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {options = a} :: AdvancedOptionsStatus)
{-# DEPRECATED aosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Specifies the status of @OptionStatus@ for advanced options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosStatus :: Lens.Lens' AdvancedOptionsStatus OptionStatus
aosStatus = Lens.lens (status :: AdvancedOptionsStatus -> OptionStatus) (\s a -> s {status = a} :: AdvancedOptionsStatus)
{-# DEPRECATED aosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON AdvancedOptionsStatus where
  parseJSON =
    Lude.withObject
      "AdvancedOptionsStatus"
      ( \x ->
          AdvancedOptionsStatus'
            Lude.<$> (x Lude..:? "Options" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Status")
      )
