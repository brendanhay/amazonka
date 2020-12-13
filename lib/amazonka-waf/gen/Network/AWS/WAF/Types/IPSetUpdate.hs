{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.IPSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.IPSetUpdate
  ( IPSetUpdate (..),

    -- * Smart constructor
    mkIPSetUpdate,

    -- * Lenses
    isuIPSetDescriptor,
    isuAction,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.IPSetDescriptor

-- | Specifies the type of update to perform to an 'IPSet' with 'UpdateIPSet' .
--
-- /See:/ 'mkIPSetUpdate' smart constructor.
data IPSetUpdate = IPSetUpdate'
  { -- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
    ipSetDescriptor :: IPSetDescriptor,
    -- | Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
    action :: ChangeAction
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPSetUpdate' with the minimum fields required to make a request.
--
-- * 'ipSetDescriptor' - The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
-- * 'action' - Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
mkIPSetUpdate ::
  -- | 'ipSetDescriptor'
  IPSetDescriptor ->
  -- | 'action'
  ChangeAction ->
  IPSetUpdate
mkIPSetUpdate pIPSetDescriptor_ pAction_ =
  IPSetUpdate'
    { ipSetDescriptor = pIPSetDescriptor_,
      action = pAction_
    }

-- | The IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR notation) that web requests originate from.
--
-- /Note:/ Consider using 'ipSetDescriptor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isuIPSetDescriptor :: Lens.Lens' IPSetUpdate IPSetDescriptor
isuIPSetDescriptor = Lens.lens (ipSetDescriptor :: IPSetUpdate -> IPSetDescriptor) (\s a -> s {ipSetDescriptor = a} :: IPSetUpdate)
{-# DEPRECATED isuIPSetDescriptor "Use generic-lens or generic-optics with 'ipSetDescriptor' instead." #-}

-- | Specifies whether to insert or delete an IP address with 'UpdateIPSet' .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isuAction :: Lens.Lens' IPSetUpdate ChangeAction
isuAction = Lens.lens (action :: IPSetUpdate -> ChangeAction) (\s a -> s {action = a} :: IPSetUpdate)
{-# DEPRECATED isuAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.ToJSON IPSetUpdate where
  toJSON IPSetUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IPSetDescriptor" Lude..= ipSetDescriptor),
            Lude.Just ("Action" Lude..= action)
          ]
      )
