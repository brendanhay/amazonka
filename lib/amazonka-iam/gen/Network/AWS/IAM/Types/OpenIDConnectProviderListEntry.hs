{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.OpenIDConnectProviderListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.OpenIDConnectProviderListEntry
  ( OpenIDConnectProviderListEntry (..)
  -- * Smart constructor
  , mkOpenIDConnectProviderListEntry
  -- * Lenses
  , oidcpleArn
  ) where

import qualified Network.AWS.IAM.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the Amazon Resource Name (ARN) for an IAM OpenID Connect provider.
--
-- /See:/ 'mkOpenIDConnectProviderListEntry' smart constructor.
newtype OpenIDConnectProviderListEntry = OpenIDConnectProviderListEntry'
  { arn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OpenIDConnectProviderListEntry' value with any optional fields omitted.
mkOpenIDConnectProviderListEntry
    :: OpenIDConnectProviderListEntry
mkOpenIDConnectProviderListEntry
  = OpenIDConnectProviderListEntry'{arn = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oidcpleArn :: Lens.Lens' OpenIDConnectProviderListEntry (Core.Maybe Types.Arn)
oidcpleArn = Lens.field @"arn"
{-# INLINEABLE oidcpleArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.FromXML OpenIDConnectProviderListEntry where
        parseXML x
          = OpenIDConnectProviderListEntry' Core.<$> (x Core..@? "Arn")
