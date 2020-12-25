{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AccountAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AccountAttribute
  ( AccountAttribute (..),

    -- * Smart constructor
    mkAccountAttribute,

    -- * Lenses
    aaAttributeName,
    aaAttributeValues,
  )
where

import qualified Network.AWS.EC2.Types.AccountAttributeValue as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an account attribute.
--
-- /See:/ 'mkAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { -- | The name of the account attribute.
    attributeName :: Core.Maybe Types.String,
    -- | The values for the account attribute.
    attributeValues :: Core.Maybe [Types.AccountAttributeValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountAttribute' value with any optional fields omitted.
mkAccountAttribute ::
  AccountAttribute
mkAccountAttribute =
  AccountAttribute'
    { attributeName = Core.Nothing,
      attributeValues = Core.Nothing
    }

-- | The name of the account attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAttributeName :: Lens.Lens' AccountAttribute (Core.Maybe Types.String)
aaAttributeName = Lens.field @"attributeName"
{-# DEPRECATED aaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The values for the account attribute.
--
-- /Note:/ Consider using 'attributeValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAttributeValues :: Lens.Lens' AccountAttribute (Core.Maybe [Types.AccountAttributeValue])
aaAttributeValues = Lens.field @"attributeValues"
{-# DEPRECATED aaAttributeValues "Use generic-lens or generic-optics with 'attributeValues' instead." #-}

instance Core.FromXML AccountAttribute where
  parseXML x =
    AccountAttribute'
      Core.<$> (x Core..@? "attributeName")
      Core.<*> (x Core..@? "attributeValueSet" Core..<@> Core.parseXMLList "item")
