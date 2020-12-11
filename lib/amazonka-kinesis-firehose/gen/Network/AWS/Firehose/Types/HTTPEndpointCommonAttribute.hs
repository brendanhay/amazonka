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
    httpecaAttributeName,
    httpecaAttributeValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the metadata that's delivered to the specified HTTP endpoint destination.
--
-- /See:/ 'mkHTTPEndpointCommonAttribute' smart constructor.
data HTTPEndpointCommonAttribute = HTTPEndpointCommonAttribute'
  { attributeName ::
      Lude.Sensitive Lude.Text,
    attributeValue ::
      Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPEndpointCommonAttribute' with the minimum fields required to make a request.
--
-- * 'attributeName' - The name of the HTTP endpoint common attribute.
-- * 'attributeValue' - The value of the HTTP endpoint common attribute.
mkHTTPEndpointCommonAttribute ::
  -- | 'attributeName'
  Lude.Sensitive Lude.Text ->
  -- | 'attributeValue'
  Lude.Sensitive Lude.Text ->
  HTTPEndpointCommonAttribute
mkHTTPEndpointCommonAttribute pAttributeName_ pAttributeValue_ =
  HTTPEndpointCommonAttribute'
    { attributeName = pAttributeName_,
      attributeValue = pAttributeValue_
    }

-- | The name of the HTTP endpoint common attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpecaAttributeName :: Lens.Lens' HTTPEndpointCommonAttribute (Lude.Sensitive Lude.Text)
httpecaAttributeName = Lens.lens (attributeName :: HTTPEndpointCommonAttribute -> Lude.Sensitive Lude.Text) (\s a -> s {attributeName = a} :: HTTPEndpointCommonAttribute)
{-# DEPRECATED httpecaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The value of the HTTP endpoint common attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpecaAttributeValue :: Lens.Lens' HTTPEndpointCommonAttribute (Lude.Sensitive Lude.Text)
httpecaAttributeValue = Lens.lens (attributeValue :: HTTPEndpointCommonAttribute -> Lude.Sensitive Lude.Text) (\s a -> s {attributeValue = a} :: HTTPEndpointCommonAttribute)
{-# DEPRECATED httpecaAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

instance Lude.FromJSON HTTPEndpointCommonAttribute where
  parseJSON =
    Lude.withObject
      "HTTPEndpointCommonAttribute"
      ( \x ->
          HTTPEndpointCommonAttribute'
            Lude.<$> (x Lude..: "AttributeName") Lude.<*> (x Lude..: "AttributeValue")
      )

instance Lude.ToJSON HTTPEndpointCommonAttribute where
  toJSON HTTPEndpointCommonAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeName" Lude..= attributeName),
            Lude.Just ("AttributeValue" Lude..= attributeValue)
          ]
      )
