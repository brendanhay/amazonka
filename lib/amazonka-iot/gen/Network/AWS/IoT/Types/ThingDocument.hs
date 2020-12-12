{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingDocument
  ( ThingDocument (..),

    -- * Smart constructor
    mkThingDocument,

    -- * Lenses
    tdThingGroupNames,
    tdThingTypeName,
    tdShadow,
    tdAttributes,
    tdConnectivity,
    tdThingName,
    tdThingId,
  )
where

import Network.AWS.IoT.Types.ThingConnectivity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The thing search index document.
--
-- /See:/ 'mkThingDocument' smart constructor.
data ThingDocument = ThingDocument'
  { thingGroupNames ::
      Lude.Maybe [Lude.Text],
    thingTypeName :: Lude.Maybe Lude.Text,
    shadow :: Lude.Maybe Lude.Text,
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    connectivity :: Lude.Maybe ThingConnectivity,
    thingName :: Lude.Maybe Lude.Text,
    thingId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingDocument' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes.
-- * 'connectivity' - Indicates whether the thing is connected to the AWS IoT service.
-- * 'shadow' - The shadow.
-- * 'thingGroupNames' - Thing group names.
-- * 'thingId' - The thing ID.
-- * 'thingName' - The thing name.
-- * 'thingTypeName' - The thing type name.
mkThingDocument ::
  ThingDocument
mkThingDocument =
  ThingDocument'
    { thingGroupNames = Lude.Nothing,
      thingTypeName = Lude.Nothing,
      shadow = Lude.Nothing,
      attributes = Lude.Nothing,
      connectivity = Lude.Nothing,
      thingName = Lude.Nothing,
      thingId = Lude.Nothing
    }

-- | Thing group names.
--
-- /Note:/ Consider using 'thingGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdThingGroupNames :: Lens.Lens' ThingDocument (Lude.Maybe [Lude.Text])
tdThingGroupNames = Lens.lens (thingGroupNames :: ThingDocument -> Lude.Maybe [Lude.Text]) (\s a -> s {thingGroupNames = a} :: ThingDocument)
{-# DEPRECATED tdThingGroupNames "Use generic-lens or generic-optics with 'thingGroupNames' instead." #-}

-- | The thing type name.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdThingTypeName :: Lens.Lens' ThingDocument (Lude.Maybe Lude.Text)
tdThingTypeName = Lens.lens (thingTypeName :: ThingDocument -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: ThingDocument)
{-# DEPRECATED tdThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The shadow.
--
-- /Note:/ Consider using 'shadow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdShadow :: Lens.Lens' ThingDocument (Lude.Maybe Lude.Text)
tdShadow = Lens.lens (shadow :: ThingDocument -> Lude.Maybe Lude.Text) (\s a -> s {shadow = a} :: ThingDocument)
{-# DEPRECATED tdShadow "Use generic-lens or generic-optics with 'shadow' instead." #-}

-- | The attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAttributes :: Lens.Lens' ThingDocument (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tdAttributes = Lens.lens (attributes :: ThingDocument -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: ThingDocument)
{-# DEPRECATED tdAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Indicates whether the thing is connected to the AWS IoT service.
--
-- /Note:/ Consider using 'connectivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdConnectivity :: Lens.Lens' ThingDocument (Lude.Maybe ThingConnectivity)
tdConnectivity = Lens.lens (connectivity :: ThingDocument -> Lude.Maybe ThingConnectivity) (\s a -> s {connectivity = a} :: ThingDocument)
{-# DEPRECATED tdConnectivity "Use generic-lens or generic-optics with 'connectivity' instead." #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdThingName :: Lens.Lens' ThingDocument (Lude.Maybe Lude.Text)
tdThingName = Lens.lens (thingName :: ThingDocument -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: ThingDocument)
{-# DEPRECATED tdThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The thing ID.
--
-- /Note:/ Consider using 'thingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdThingId :: Lens.Lens' ThingDocument (Lude.Maybe Lude.Text)
tdThingId = Lens.lens (thingId :: ThingDocument -> Lude.Maybe Lude.Text) (\s a -> s {thingId = a} :: ThingDocument)
{-# DEPRECATED tdThingId "Use generic-lens or generic-optics with 'thingId' instead." #-}

instance Lude.FromJSON ThingDocument where
  parseJSON =
    Lude.withObject
      "ThingDocument"
      ( \x ->
          ThingDocument'
            Lude.<$> (x Lude..:? "thingGroupNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "thingTypeName")
            Lude.<*> (x Lude..:? "shadow")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "connectivity")
            Lude.<*> (x Lude..:? "thingName")
            Lude.<*> (x Lude..:? "thingId")
      )
