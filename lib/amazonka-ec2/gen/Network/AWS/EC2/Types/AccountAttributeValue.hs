{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AccountAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AccountAttributeValue
  ( AccountAttributeValue (..),

    -- * Smart constructor
    mkAccountAttributeValue,

    -- * Lenses
    aavAttributeValue,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a value of an account attribute.
--
-- /See:/ 'mkAccountAttributeValue' smart constructor.
newtype AccountAttributeValue = AccountAttributeValue'
  { -- | The value of the attribute.
    attributeValue :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AccountAttributeValue' value with any optional fields omitted.
mkAccountAttributeValue ::
  AccountAttributeValue
mkAccountAttributeValue =
  AccountAttributeValue' {attributeValue = Core.Nothing}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aavAttributeValue :: Lens.Lens' AccountAttributeValue (Core.Maybe Types.String)
aavAttributeValue = Lens.field @"attributeValue"
{-# DEPRECATED aavAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

instance Core.FromXML AccountAttributeValue where
  parseXML x =
    AccountAttributeValue' Core.<$> (x Core..@? "attributeValue")
