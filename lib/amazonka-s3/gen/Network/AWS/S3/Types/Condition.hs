{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Condition
  ( Condition (..),

    -- * Smart constructor
    mkCondition,

    -- * Lenses
    cHttpErrorCodeReturnedEquals,
    cKeyPrefixEquals,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.HttpErrorCodeReturnedEquals as Types
import qualified Network.AWS.S3.Types.KeyPrefixEquals as Types

-- | A container for describing a condition that must be met for the specified redirect to apply. For example, 1. If request is for pages in the @/docs@ folder, redirect to the @/documents@ folder. 2. If request results in HTTP error 4xx, redirect request to another host where you might process the error.
--
-- /See:/ 'mkCondition' smart constructor.
data Condition = Condition'
  { -- | The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
    httpErrorCodeReturnedEquals :: Core.Maybe Types.HttpErrorCodeReturnedEquals,
    -- | The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
    keyPrefixEquals :: Core.Maybe Types.KeyPrefixEquals
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Condition' value with any optional fields omitted.
mkCondition ::
  Condition
mkCondition =
  Condition'
    { httpErrorCodeReturnedEquals = Core.Nothing,
      keyPrefixEquals = Core.Nothing
    }

-- | The HTTP error code when the redirect is applied. In the event of an error, if the error code equals this value, then the specified redirect is applied. Required when parent element @Condition@ is specified and sibling @KeyPrefixEquals@ is not specified. If both are specified, then both must be true for the redirect to be applied.
--
-- /Note:/ Consider using 'httpErrorCodeReturnedEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHttpErrorCodeReturnedEquals :: Lens.Lens' Condition (Core.Maybe Types.HttpErrorCodeReturnedEquals)
cHttpErrorCodeReturnedEquals = Lens.field @"httpErrorCodeReturnedEquals"
{-# DEPRECATED cHttpErrorCodeReturnedEquals "Use generic-lens or generic-optics with 'httpErrorCodeReturnedEquals' instead." #-}

-- | The object key name prefix when the redirect is applied. For example, to redirect requests for @ExamplePage.html@ , the key prefix will be @ExamplePage.html@ . To redirect request for all pages with the prefix @docs/@ , the key prefix will be @/docs@ , which identifies all objects in the @docs/@ folder. Required when the parent element @Condition@ is specified and sibling @HttpErrorCodeReturnedEquals@ is not specified. If both conditions are specified, both must be true for the redirect to be applied.
--
-- /Note:/ Consider using 'keyPrefixEquals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKeyPrefixEquals :: Lens.Lens' Condition (Core.Maybe Types.KeyPrefixEquals)
cKeyPrefixEquals = Lens.field @"keyPrefixEquals"
{-# DEPRECATED cKeyPrefixEquals "Use generic-lens or generic-optics with 'keyPrefixEquals' instead." #-}

instance Core.ToXML Condition where
  toXML Condition {..} =
    Core.toXMLNode "HttpErrorCodeReturnedEquals"
      Core.<$> httpErrorCodeReturnedEquals
      Core.<> Core.toXMLNode "KeyPrefixEquals" Core.<$> keyPrefixEquals

instance Core.FromXML Condition where
  parseXML x =
    Condition'
      Core.<$> (x Core..@? "HttpErrorCodeReturnedEquals")
      Core.<*> (x Core..@? "KeyPrefixEquals")
