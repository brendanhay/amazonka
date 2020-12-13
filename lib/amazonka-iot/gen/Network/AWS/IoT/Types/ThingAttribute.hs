{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingAttribute
  ( ThingAttribute (..),

    -- * Smart constructor
    mkThingAttribute,

    -- * Lenses
    taThingTypeName,
    taThingARN,
    taAttributes,
    taVersion,
    taThingName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The properties of the thing, including thing name, thing type name, and a list of thing attributes.
--
-- /See:/ 'mkThingAttribute' smart constructor.
data ThingAttribute = ThingAttribute'
  { -- | The name of the thing type, if the thing has been associated with a type.
    thingTypeName :: Lude.Maybe Lude.Text,
    -- | The thing ARN.
    thingARN :: Lude.Maybe Lude.Text,
    -- | A list of thing attributes which are name-value pairs.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The version of the thing record in the registry.
    version :: Lude.Maybe Lude.Integer,
    -- | The name of the thing.
    thingName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingAttribute' with the minimum fields required to make a request.
--
-- * 'thingTypeName' - The name of the thing type, if the thing has been associated with a type.
-- * 'thingARN' - The thing ARN.
-- * 'attributes' - A list of thing attributes which are name-value pairs.
-- * 'version' - The version of the thing record in the registry.
-- * 'thingName' - The name of the thing.
mkThingAttribute ::
  ThingAttribute
mkThingAttribute =
  ThingAttribute'
    { thingTypeName = Lude.Nothing,
      thingARN = Lude.Nothing,
      attributes = Lude.Nothing,
      version = Lude.Nothing,
      thingName = Lude.Nothing
    }

-- | The name of the thing type, if the thing has been associated with a type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taThingTypeName :: Lens.Lens' ThingAttribute (Lude.Maybe Lude.Text)
taThingTypeName = Lens.lens (thingTypeName :: ThingAttribute -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: ThingAttribute)
{-# DEPRECATED taThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The thing ARN.
--
-- /Note:/ Consider using 'thingARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taThingARN :: Lens.Lens' ThingAttribute (Lude.Maybe Lude.Text)
taThingARN = Lens.lens (thingARN :: ThingAttribute -> Lude.Maybe Lude.Text) (\s a -> s {thingARN = a} :: ThingAttribute)
{-# DEPRECATED taThingARN "Use generic-lens or generic-optics with 'thingARN' instead." #-}

-- | A list of thing attributes which are name-value pairs.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taAttributes :: Lens.Lens' ThingAttribute (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
taAttributes = Lens.lens (attributes :: ThingAttribute -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: ThingAttribute)
{-# DEPRECATED taAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The version of the thing record in the registry.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taVersion :: Lens.Lens' ThingAttribute (Lude.Maybe Lude.Integer)
taVersion = Lens.lens (version :: ThingAttribute -> Lude.Maybe Lude.Integer) (\s a -> s {version = a} :: ThingAttribute)
{-# DEPRECATED taVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taThingName :: Lens.Lens' ThingAttribute (Lude.Maybe Lude.Text)
taThingName = Lens.lens (thingName :: ThingAttribute -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: ThingAttribute)
{-# DEPRECATED taThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.FromJSON ThingAttribute where
  parseJSON =
    Lude.withObject
      "ThingAttribute"
      ( \x ->
          ThingAttribute'
            Lude.<$> (x Lude..:? "thingTypeName")
            Lude.<*> (x Lude..:? "thingArn")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "thingName")
      )
