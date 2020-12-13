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
    gckfpContextKeyNames,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the response to a successful 'GetContextKeysForPrincipalPolicy' or 'GetContextKeysForCustomPolicy' request.
--
-- /See:/ 'mkGetContextKeysForPolicyResponse' smart constructor.
newtype GetContextKeysForPolicyResponse = GetContextKeysForPolicyResponse'
  { -- | The list of context keys that are referenced in the input policies.
    contextKeyNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContextKeysForPolicyResponse' with the minimum fields required to make a request.
--
-- * 'contextKeyNames' - The list of context keys that are referenced in the input policies.
mkGetContextKeysForPolicyResponse ::
  GetContextKeysForPolicyResponse
mkGetContextKeysForPolicyResponse =
  GetContextKeysForPolicyResponse' {contextKeyNames = Lude.Nothing}

-- | The list of context keys that are referenced in the input policies.
--
-- /Note:/ Consider using 'contextKeyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gckfpContextKeyNames :: Lens.Lens' GetContextKeysForPolicyResponse (Lude.Maybe [Lude.Text])
gckfpContextKeyNames = Lens.lens (contextKeyNames :: GetContextKeysForPolicyResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {contextKeyNames = a} :: GetContextKeysForPolicyResponse)
{-# DEPRECATED gckfpContextKeyNames "Use generic-lens or generic-optics with 'contextKeyNames' instead." #-}

instance Lude.FromXML GetContextKeysForPolicyResponse where
  parseXML x =
    GetContextKeysForPolicyResponse'
      Lude.<$> ( x Lude..@? "ContextKeyNames" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
