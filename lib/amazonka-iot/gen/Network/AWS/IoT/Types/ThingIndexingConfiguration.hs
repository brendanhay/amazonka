{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingIndexingConfiguration
  ( ThingIndexingConfiguration (..),

    -- * Smart constructor
    mkThingIndexingConfiguration,

    -- * Lenses
    ticManagedFields,
    ticThingConnectivityIndexingMode,
    ticCustomFields,
    ticThingIndexingMode,
  )
where

import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.ThingConnectivityIndexingMode
import Network.AWS.IoT.Types.ThingIndexingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The thing indexing configuration. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/managing-index.html Managing Thing Indexing> .
--
-- /See:/ 'mkThingIndexingConfiguration' smart constructor.
data ThingIndexingConfiguration = ThingIndexingConfiguration'
  { managedFields ::
      Lude.Maybe [Field],
    thingConnectivityIndexingMode ::
      Lude.Maybe
        ThingConnectivityIndexingMode,
    customFields :: Lude.Maybe [Field],
    thingIndexingMode ::
      ThingIndexingMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingIndexingConfiguration' with the minimum fields required to make a request.
--
-- * 'customFields' - Contains custom field names and their data type.
-- * 'managedFields' - Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
-- * 'thingConnectivityIndexingMode' - Thing connectivity indexing mode. Valid values are:
--
--
--     * STATUS – Your thing index contains connectivity status. To enable thing connectivity indexing, thingIndexMode must not be set to OFF.
--
--
--     * OFF - Thing connectivity status indexing is disabled.
--
--
-- * 'thingIndexingMode' - Thing indexing mode. Valid values are:
--
--
--     * REGISTRY – Your thing index contains registry data only.
--
--
--     * REGISTRY_AND_SHADOW - Your thing index contains registry and shadow data.
--
--
--     * OFF - Thing indexing is disabled.
mkThingIndexingConfiguration ::
  -- | 'thingIndexingMode'
  ThingIndexingMode ->
  ThingIndexingConfiguration
mkThingIndexingConfiguration pThingIndexingMode_ =
  ThingIndexingConfiguration'
    { managedFields = Lude.Nothing,
      thingConnectivityIndexingMode = Lude.Nothing,
      customFields = Lude.Nothing,
      thingIndexingMode = pThingIndexingMode_
    }

-- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
--
-- /Note:/ Consider using 'managedFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ticManagedFields :: Lens.Lens' ThingIndexingConfiguration (Lude.Maybe [Field])
ticManagedFields = Lens.lens (managedFields :: ThingIndexingConfiguration -> Lude.Maybe [Field]) (\s a -> s {managedFields = a} :: ThingIndexingConfiguration)
{-# DEPRECATED ticManagedFields "Use generic-lens or generic-optics with 'managedFields' instead." #-}

-- | Thing connectivity indexing mode. Valid values are:
--
--
--     * STATUS – Your thing index contains connectivity status. To enable thing connectivity indexing, thingIndexMode must not be set to OFF.
--
--
--     * OFF - Thing connectivity status indexing is disabled.
--
--
--
-- /Note:/ Consider using 'thingConnectivityIndexingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ticThingConnectivityIndexingMode :: Lens.Lens' ThingIndexingConfiguration (Lude.Maybe ThingConnectivityIndexingMode)
ticThingConnectivityIndexingMode = Lens.lens (thingConnectivityIndexingMode :: ThingIndexingConfiguration -> Lude.Maybe ThingConnectivityIndexingMode) (\s a -> s {thingConnectivityIndexingMode = a} :: ThingIndexingConfiguration)
{-# DEPRECATED ticThingConnectivityIndexingMode "Use generic-lens or generic-optics with 'thingConnectivityIndexingMode' instead." #-}

-- | Contains custom field names and their data type.
--
-- /Note:/ Consider using 'customFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ticCustomFields :: Lens.Lens' ThingIndexingConfiguration (Lude.Maybe [Field])
ticCustomFields = Lens.lens (customFields :: ThingIndexingConfiguration -> Lude.Maybe [Field]) (\s a -> s {customFields = a} :: ThingIndexingConfiguration)
{-# DEPRECATED ticCustomFields "Use generic-lens or generic-optics with 'customFields' instead." #-}

-- | Thing indexing mode. Valid values are:
--
--
--     * REGISTRY – Your thing index contains registry data only.
--
--
--     * REGISTRY_AND_SHADOW - Your thing index contains registry and shadow data.
--
--
--     * OFF - Thing indexing is disabled.
--
--
--
-- /Note:/ Consider using 'thingIndexingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ticThingIndexingMode :: Lens.Lens' ThingIndexingConfiguration ThingIndexingMode
ticThingIndexingMode = Lens.lens (thingIndexingMode :: ThingIndexingConfiguration -> ThingIndexingMode) (\s a -> s {thingIndexingMode = a} :: ThingIndexingConfiguration)
{-# DEPRECATED ticThingIndexingMode "Use generic-lens or generic-optics with 'thingIndexingMode' instead." #-}

instance Lude.FromJSON ThingIndexingConfiguration where
  parseJSON =
    Lude.withObject
      "ThingIndexingConfiguration"
      ( \x ->
          ThingIndexingConfiguration'
            Lude.<$> (x Lude..:? "managedFields" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "thingConnectivityIndexingMode")
            Lude.<*> (x Lude..:? "customFields" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "thingIndexingMode")
      )

instance Lude.ToJSON ThingIndexingConfiguration where
  toJSON ThingIndexingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("managedFields" Lude..=) Lude.<$> managedFields,
            ("thingConnectivityIndexingMode" Lude..=)
              Lude.<$> thingConnectivityIndexingMode,
            ("customFields" Lude..=) Lude.<$> customFields,
            Lude.Just ("thingIndexingMode" Lude..= thingIndexingMode)
          ]
      )
