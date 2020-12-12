{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Limits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.Limits
  ( Limits (..),

    -- * Smart constructor
    mkLimits,

    -- * Lenses
    lInstanceLimits,
    lAdditionalLimits,
    lStorageTypes,
  )
where

import Network.AWS.ElasticSearch.Types.AdditionalLimit
import Network.AWS.ElasticSearch.Types.InstanceLimits
import Network.AWS.ElasticSearch.Types.StorageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Limits for given InstanceType and for each of it's role.
--
-- Limits contains following @'StorageTypes,' @ @'InstanceLimits' @ and @'AdditionalLimits' @
--
-- /See:/ 'mkLimits' smart constructor.
data Limits = Limits'
  { instanceLimits :: Lude.Maybe InstanceLimits,
    additionalLimits :: Lude.Maybe [AdditionalLimit],
    storageTypes :: Lude.Maybe [StorageType]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Limits' with the minimum fields required to make a request.
--
-- * 'additionalLimits' - List of additional limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ .
-- * 'instanceLimits' - Undocumented field.
-- * 'storageTypes' - StorageType represents the list of storage related types and attributes that are available for given InstanceType.
mkLimits ::
  Limits
mkLimits =
  Limits'
    { instanceLimits = Lude.Nothing,
      additionalLimits = Lude.Nothing,
      storageTypes = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'instanceLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lInstanceLimits :: Lens.Lens' Limits (Lude.Maybe InstanceLimits)
lInstanceLimits = Lens.lens (instanceLimits :: Limits -> Lude.Maybe InstanceLimits) (\s a -> s {instanceLimits = a} :: Limits)
{-# DEPRECATED lInstanceLimits "Use generic-lens or generic-optics with 'instanceLimits' instead." #-}

-- | List of additional limits that are specific to a given InstanceType and for each of it's @'InstanceRole' @ .
--
-- /Note:/ Consider using 'additionalLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAdditionalLimits :: Lens.Lens' Limits (Lude.Maybe [AdditionalLimit])
lAdditionalLimits = Lens.lens (additionalLimits :: Limits -> Lude.Maybe [AdditionalLimit]) (\s a -> s {additionalLimits = a} :: Limits)
{-# DEPRECATED lAdditionalLimits "Use generic-lens or generic-optics with 'additionalLimits' instead." #-}

-- | StorageType represents the list of storage related types and attributes that are available for given InstanceType.
--
-- /Note:/ Consider using 'storageTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lStorageTypes :: Lens.Lens' Limits (Lude.Maybe [StorageType])
lStorageTypes = Lens.lens (storageTypes :: Limits -> Lude.Maybe [StorageType]) (\s a -> s {storageTypes = a} :: Limits)
{-# DEPRECATED lStorageTypes "Use generic-lens or generic-optics with 'storageTypes' instead." #-}

instance Lude.FromJSON Limits where
  parseJSON =
    Lude.withObject
      "Limits"
      ( \x ->
          Limits'
            Lude.<$> (x Lude..:? "InstanceLimits")
            Lude.<*> (x Lude..:? "AdditionalLimits" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "StorageTypes" Lude..!= Lude.mempty)
      )
