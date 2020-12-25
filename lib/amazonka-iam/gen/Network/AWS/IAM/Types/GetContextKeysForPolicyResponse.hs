{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.GetContextKeysForPolicyResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.GetContextKeysForPolicyResponse
  ( GetContextKeysForPolicyResponse (..),

    -- * Smart constructor
    mkGetContextKeysForPolicyResponse,

    -- * Lenses
    gckfprContextKeyNames,
  )
where

import qualified Network.AWS.IAM.Types.ContextKeyNameType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the response to a successful 'GetContextKeysForPrincipalPolicy' or 'GetContextKeysForCustomPolicy' request.
--
-- /See:/ 'mkGetContextKeysForPolicyResponse' smart constructor.
newtype GetContextKeysForPolicyResponse = GetContextKeysForPolicyResponse'
  { -- | The list of context keys that are referenced in the input policies.
    contextKeyNames :: Core.Maybe [Types.ContextKeyNameType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContextKeysForPolicyResponse' value with any optional fields omitted.
mkGetContextKeysForPolicyResponse ::
  GetContextKeysForPolicyResponse
mkGetContextKeysForPolicyResponse =
  GetContextKeysForPolicyResponse' {contextKeyNames = Core.Nothing}

-- | The list of context keys that are referenced in the input policies.
--
-- /Note:/ Consider using 'contextKeyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gckfprContextKeyNames :: Lens.Lens' GetContextKeysForPolicyResponse (Core.Maybe [Types.ContextKeyNameType])
gckfprContextKeyNames = Lens.field @"contextKeyNames"
{-# DEPRECATED gckfprContextKeyNames "Use generic-lens or generic-optics with 'contextKeyNames' instead." #-}

instance Core.FromXML GetContextKeysForPolicyResponse where
  parseXML x =
    GetContextKeysForPolicyResponse'
      Core.<$> (x Core..@? "ContextKeyNames" Core..<@> Core.parseXMLList "member")
