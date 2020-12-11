-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
  ( DomainEndpointOptionsStatus (..),

    -- * Smart constructor
    mkDomainEndpointOptionsStatus,

    -- * Lenses
    deosOptions,
    deosStatus,
  )
where

import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configured endpoint options for the domain and their current status.
--
-- /See:/ 'mkDomainEndpointOptionsStatus' smart constructor.
data DomainEndpointOptionsStatus = DomainEndpointOptionsStatus'
  { options ::
      DomainEndpointOptions,
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

-- | Creates a value of 'DomainEndpointOptionsStatus' with the minimum fields required to make a request.
--
-- * 'options' - Options to configure endpoint for the Elasticsearch domain.
-- * 'status' - The status of the endpoint options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
mkDomainEndpointOptionsStatus ::
  -- | 'options'
  DomainEndpointOptions ->
  -- | 'status'
  OptionStatus ->
  DomainEndpointOptionsStatus
mkDomainEndpointOptionsStatus pOptions_ pStatus_ =
  DomainEndpointOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Options to configure endpoint for the Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deosOptions :: Lens.Lens' DomainEndpointOptionsStatus DomainEndpointOptions
deosOptions = Lens.lens (options :: DomainEndpointOptionsStatus -> DomainEndpointOptions) (\s a -> s {options = a} :: DomainEndpointOptionsStatus)
{-# DEPRECATED deosOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The status of the endpoint options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deosStatus :: Lens.Lens' DomainEndpointOptionsStatus OptionStatus
deosStatus = Lens.lens (status :: DomainEndpointOptionsStatus -> OptionStatus) (\s a -> s {status = a} :: DomainEndpointOptionsStatus)
{-# DEPRECATED deosStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON DomainEndpointOptionsStatus where
  parseJSON =
    Lude.withObject
      "DomainEndpointOptionsStatus"
      ( \x ->
          DomainEndpointOptionsStatus'
            Lude.<$> (x Lude..: "Options") Lude.<*> (x Lude..: "Status")
      )
