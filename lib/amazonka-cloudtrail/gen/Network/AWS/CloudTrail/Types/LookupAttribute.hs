-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.LookupAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.LookupAttribute
  ( LookupAttribute (..),

    -- * Smart constructor
    mkLookupAttribute,

    -- * Lenses
    laAttributeKey,
    laAttributeValue,
  )
where

import Network.AWS.CloudTrail.Types.LookupAttributeKey
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an attribute and value that filter the events returned.
--
-- /See:/ 'mkLookupAttribute' smart constructor.
data LookupAttribute = LookupAttribute'
  { attributeKey ::
      LookupAttributeKey,
    attributeValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LookupAttribute' with the minimum fields required to make a request.
--
-- * 'attributeKey' - Specifies an attribute on which to filter the events returned.
-- * 'attributeValue' - Specifies a value for the specified AttributeKey.
mkLookupAttribute ::
  -- | 'attributeKey'
  LookupAttributeKey ->
  -- | 'attributeValue'
  Lude.Text ->
  LookupAttribute
mkLookupAttribute pAttributeKey_ pAttributeValue_ =
  LookupAttribute'
    { attributeKey = pAttributeKey_,
      attributeValue = pAttributeValue_
    }

-- | Specifies an attribute on which to filter the events returned.
--
-- /Note:/ Consider using 'attributeKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAttributeKey :: Lens.Lens' LookupAttribute LookupAttributeKey
laAttributeKey = Lens.lens (attributeKey :: LookupAttribute -> LookupAttributeKey) (\s a -> s {attributeKey = a} :: LookupAttribute)
{-# DEPRECATED laAttributeKey "Use generic-lens or generic-optics with 'attributeKey' instead." #-}

-- | Specifies a value for the specified AttributeKey.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laAttributeValue :: Lens.Lens' LookupAttribute Lude.Text
laAttributeValue = Lens.lens (attributeValue :: LookupAttribute -> Lude.Text) (\s a -> s {attributeValue = a} :: LookupAttribute)
{-# DEPRECATED laAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

instance Lude.ToJSON LookupAttribute where
  toJSON LookupAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeKey" Lude..= attributeKey),
            Lude.Just ("AttributeValue" Lude..= attributeValue)
          ]
      )
