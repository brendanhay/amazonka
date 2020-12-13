{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointCommonAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointCommonAttribute
  ( HTTPEndpointCommonAttribute (..),

    -- * Smart constructor
    mkHTTPEndpointCommonAttribute,

    -- * Lenses
    httpecaAttributeValue,
    httpecaAttributeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the metadata that's delivered to the specified HTTP endpoint destination.
--
-- /See:/ 'mkHTTPEndpointCommonAttribute' smart constructor.
data HTTPEndpointCommonAttribute = HTTPEndpointCommonAttribute'
  { -- | The value of the HTTP endpoint common attribute.
    attributeValue :: Lude.Sensitive Lude.Text,
    -- | The name of the HTTP endpoint common attribute.
    attributeName :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointCommonAttribute' with the minimum fields required to make a request.
--
-- * 'attributeValue' - The value of the HTTP endpoint common attribute.
-- * 'attributeName' - The name of the HTTP endpoint common attribute.
mkHTTPEndpointCommonAttribute ::
  -- | 'attributeValue'
  Lude.Sensitive Lude.Text ->
  -- | 'attributeName'
  Lude.Sensitive Lude.Text ->
  HTTPEndpointCommonAttribute
mkHTTPEndpointCommonAttribute pAttributeValue_ pAttributeName_ =
  HTTPEndpointCommonAttribute'
    { attributeValue = pAttributeValue_,
      attributeName = pAttributeName_
    }

-- | The value of the HTTP endpoint common attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpecaAttributeValue :: Lens.Lens' HTTPEndpointCommonAttribute (Lude.Sensitive Lude.Text)
httpecaAttributeValue = Lens.lens (attributeValue :: HTTPEndpointCommonAttribute -> Lude.Sensitive Lude.Text) (\s a -> s {attributeValue = a} :: HTTPEndpointCommonAttribute)
{-# DEPRECATED httpecaAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The name of the HTTP endpoint common attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpecaAttributeName :: Lens.Lens' HTTPEndpointCommonAttribute (Lude.Sensitive Lude.Text)
httpecaAttributeName = Lens.lens (attributeName :: HTTPEndpointCommonAttribute -> Lude.Sensitive Lude.Text) (\s a -> s {attributeName = a} :: HTTPEndpointCommonAttribute)
{-# DEPRECATED httpecaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromJSON HTTPEndpointCommonAttribute where
  parseJSON =
    Lude.withObject
      "HTTPEndpointCommonAttribute"
      ( \x ->
          HTTPEndpointCommonAttribute'
            Lude.<$> (x Lude..: "AttributeValue") Lude.<*> (x Lude..: "AttributeName")
      )

instance Lude.ToJSON HTTPEndpointCommonAttribute where
  toJSON HTTPEndpointCommonAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeValue" Lude..= attributeValue),
            Lude.Just ("AttributeName" Lude..= attributeName)
          ]
      )
