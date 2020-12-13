{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
  ( ThingGroupIndexingConfiguration (..),

    -- * Smart constructor
    mkThingGroupIndexingConfiguration,

    -- * Lenses
    tgicManagedFields,
    tgicCustomFields,
    tgicThingGroupIndexingMode,
  )
where

import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.ThingGroupIndexingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Thing group indexing configuration.
--
-- /See:/ 'mkThingGroupIndexingConfiguration' smart constructor.
data ThingGroupIndexingConfiguration = ThingGroupIndexingConfiguration'
  { -- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
    managedFields :: Lude.Maybe [Field],
    -- | A list of thing group fields to index. This list cannot contain any managed fields. Use the GetIndexingConfiguration API to get a list of managed fields.
    --
    -- Contains custom field names and their data type.
    customFields :: Lude.Maybe [Field],
    -- | Thing group indexing mode.
    thingGroupIndexingMode :: ThingGroupIndexingMode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingGroupIndexingConfiguration' with the minimum fields required to make a request.
--
-- * 'managedFields' - Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
-- * 'customFields' - A list of thing group fields to index. This list cannot contain any managed fields. Use the GetIndexingConfiguration API to get a list of managed fields.
--
-- Contains custom field names and their data type.
-- * 'thingGroupIndexingMode' - Thing group indexing mode.
mkThingGroupIndexingConfiguration ::
  -- | 'thingGroupIndexingMode'
  ThingGroupIndexingMode ->
  ThingGroupIndexingConfiguration
mkThingGroupIndexingConfiguration pThingGroupIndexingMode_ =
  ThingGroupIndexingConfiguration'
    { managedFields = Lude.Nothing,
      customFields = Lude.Nothing,
      thingGroupIndexingMode = pThingGroupIndexingMode_
    }

-- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
--
-- /Note:/ Consider using 'managedFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgicManagedFields :: Lens.Lens' ThingGroupIndexingConfiguration (Lude.Maybe [Field])
tgicManagedFields = Lens.lens (managedFields :: ThingGroupIndexingConfiguration -> Lude.Maybe [Field]) (\s a -> s {managedFields = a} :: ThingGroupIndexingConfiguration)
{-# DEPRECATED tgicManagedFields "Use generic-lens or generic-optics with 'managedFields' instead." #-}

-- | A list of thing group fields to index. This list cannot contain any managed fields. Use the GetIndexingConfiguration API to get a list of managed fields.
--
-- Contains custom field names and their data type.
--
-- /Note:/ Consider using 'customFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgicCustomFields :: Lens.Lens' ThingGroupIndexingConfiguration (Lude.Maybe [Field])
tgicCustomFields = Lens.lens (customFields :: ThingGroupIndexingConfiguration -> Lude.Maybe [Field]) (\s a -> s {customFields = a} :: ThingGroupIndexingConfiguration)
{-# DEPRECATED tgicCustomFields "Use generic-lens or generic-optics with 'customFields' instead." #-}

-- | Thing group indexing mode.
--
-- /Note:/ Consider using 'thingGroupIndexingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgicThingGroupIndexingMode :: Lens.Lens' ThingGroupIndexingConfiguration ThingGroupIndexingMode
tgicThingGroupIndexingMode = Lens.lens (thingGroupIndexingMode :: ThingGroupIndexingConfiguration -> ThingGroupIndexingMode) (\s a -> s {thingGroupIndexingMode = a} :: ThingGroupIndexingConfiguration)
{-# DEPRECATED tgicThingGroupIndexingMode "Use generic-lens or generic-optics with 'thingGroupIndexingMode' instead." #-}

instance Lude.FromJSON ThingGroupIndexingConfiguration where
  parseJSON =
    Lude.withObject
      "ThingGroupIndexingConfiguration"
      ( \x ->
          ThingGroupIndexingConfiguration'
            Lude.<$> (x Lude..:? "managedFields" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "customFields" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "thingGroupIndexingMode")
      )

instance Lude.ToJSON ThingGroupIndexingConfiguration where
  toJSON ThingGroupIndexingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("managedFields" Lude..=) Lude.<$> managedFields,
            ("customFields" Lude..=) Lude.<$> customFields,
            Lude.Just
              ("thingGroupIndexingMode" Lude..= thingGroupIndexingMode)
          ]
      )
