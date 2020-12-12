{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ResourceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ResourceAttribute
  ( ResourceAttribute (..),

    -- * Smart constructor
    mkResourceAttribute,

    -- * Lenses
    raType,
    raValue,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.ResourceAttributeType
import qualified Network.AWS.Prelude as Lude

-- | Attribute associated with a resource.
--
-- Note the corresponding format required per type listed below:
--
--     * IPV4
--
--     * @x.x.x.x@
-- /where x is an integer in the range [0,255]/
--
--
--     * IPV6
--
--     * @y : y : y : y : y : y : y : y@
-- /where y is a hexadecimal between 0 and FFFF. [0, FFFF]/
--
--
--     * MAC_ADDRESS
--
--     * @^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$@
--
--
--     * FQDN
--
--     * @^[^<>{}\\\\/?,=\\p{Cntrl}]{1,256}$@
--
--
--
-- /See:/ 'mkResourceAttribute' smart constructor.
data ResourceAttribute = ResourceAttribute'
  { type' ::
      ResourceAttributeType,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceAttribute' with the minimum fields required to make a request.
--
-- * 'type'' - Type of resource.
-- * 'value' - Value of the resource type.
mkResourceAttribute ::
  -- | 'type''
  ResourceAttributeType ->
  -- | 'value'
  Lude.Text ->
  ResourceAttribute
mkResourceAttribute pType_ pValue_ =
  ResourceAttribute' {type' = pType_, value = pValue_}

-- | Type of resource.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raType :: Lens.Lens' ResourceAttribute ResourceAttributeType
raType = Lens.lens (type' :: ResourceAttribute -> ResourceAttributeType) (\s a -> s {type' = a} :: ResourceAttribute)
{-# DEPRECATED raType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Value of the resource type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raValue :: Lens.Lens' ResourceAttribute Lude.Text
raValue = Lens.lens (value :: ResourceAttribute -> Lude.Text) (\s a -> s {value = a} :: ResourceAttribute)
{-# DEPRECATED raValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON ResourceAttribute where
  parseJSON =
    Lude.withObject
      "ResourceAttribute"
      ( \x ->
          ResourceAttribute'
            Lude.<$> (x Lude..: "Type") Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON ResourceAttribute where
  toJSON ResourceAttribute' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Value" Lude..= value)
          ]
      )
