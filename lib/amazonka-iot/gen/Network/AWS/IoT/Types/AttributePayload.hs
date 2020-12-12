{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AttributePayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AttributePayload
  ( AttributePayload (..),

    -- * Smart constructor
    mkAttributePayload,

    -- * Lenses
    apAttributes,
    apMerge,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The attribute payload.
--
-- /See:/ 'mkAttributePayload' smart constructor.
data AttributePayload = AttributePayload'
  { attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    merge :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributePayload' with the minimum fields required to make a request.
--
-- * 'attributes' - A JSON string containing up to three key-value pair in JSON format. For example:
--
-- @{\"attributes\":{\"string1\":\"string2\"}}@
-- * 'merge' - Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them.
--
-- To remove an attribute, call @UpdateThing@ with an empty attribute value.
mkAttributePayload ::
  AttributePayload
mkAttributePayload =
  AttributePayload'
    { attributes = Lude.Nothing,
      merge = Lude.Nothing
    }

-- | A JSON string containing up to three key-value pair in JSON format. For example:
--
-- @{\"attributes\":{\"string1\":\"string2\"}}@
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAttributes :: Lens.Lens' AttributePayload (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
apAttributes = Lens.lens (attributes :: AttributePayload -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: AttributePayload)
{-# DEPRECATED apAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Specifies whether the list of attributes provided in the @AttributePayload@ is merged with the attributes stored in the registry, instead of overwriting them.
--
-- To remove an attribute, call @UpdateThing@ with an empty attribute value.
--
-- /Note:/ Consider using 'merge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apMerge :: Lens.Lens' AttributePayload (Lude.Maybe Lude.Bool)
apMerge = Lens.lens (merge :: AttributePayload -> Lude.Maybe Lude.Bool) (\s a -> s {merge = a} :: AttributePayload)
{-# DEPRECATED apMerge "Use generic-lens or generic-optics with 'merge' instead." #-}

instance Lude.FromJSON AttributePayload where
  parseJSON =
    Lude.withObject
      "AttributePayload"
      ( \x ->
          AttributePayload'
            Lude.<$> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "merge")
      )

instance Lude.ToJSON AttributePayload where
  toJSON AttributePayload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("attributes" Lude..=) Lude.<$> attributes,
            ("merge" Lude..=) Lude.<$> merge
          ]
      )
