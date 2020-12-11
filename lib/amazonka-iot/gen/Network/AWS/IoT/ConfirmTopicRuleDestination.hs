{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ConfirmTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms a topic rule destination. When you create a rule requiring a destination, AWS IoT sends a confirmation message to the endpoint or base address you specify. The message includes a token which you pass back when calling @ConfirmTopicRuleDestination@ to confirm that you own or have access to the endpoint.
module Network.AWS.IoT.ConfirmTopicRuleDestination
  ( -- * Creating a request
    ConfirmTopicRuleDestination (..),
    mkConfirmTopicRuleDestination,

    -- ** Request lenses
    ctrdConfirmationToken,

    -- * Destructuring the response
    ConfirmTopicRuleDestinationResponse (..),
    mkConfirmTopicRuleDestinationResponse,

    -- ** Response lenses
    ctrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkConfirmTopicRuleDestination' smart constructor.
newtype ConfirmTopicRuleDestination = ConfirmTopicRuleDestination'
  { confirmationToken ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmTopicRuleDestination' with the minimum fields required to make a request.
--
-- * 'confirmationToken' - The token used to confirm ownership or access to the topic rule confirmation URL.
mkConfirmTopicRuleDestination ::
  -- | 'confirmationToken'
  Lude.Text ->
  ConfirmTopicRuleDestination
mkConfirmTopicRuleDestination pConfirmationToken_ =
  ConfirmTopicRuleDestination'
    { confirmationToken =
        pConfirmationToken_
    }

-- | The token used to confirm ownership or access to the topic rule confirmation URL.
--
-- /Note:/ Consider using 'confirmationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrdConfirmationToken :: Lens.Lens' ConfirmTopicRuleDestination Lude.Text
ctrdConfirmationToken = Lens.lens (confirmationToken :: ConfirmTopicRuleDestination -> Lude.Text) (\s a -> s {confirmationToken = a} :: ConfirmTopicRuleDestination)
{-# DEPRECATED ctrdConfirmationToken "Use generic-lens or generic-optics with 'confirmationToken' instead." #-}

instance Lude.AWSRequest ConfirmTopicRuleDestination where
  type
    Rs ConfirmTopicRuleDestination =
      ConfirmTopicRuleDestinationResponse
  request = Req.get ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ConfirmTopicRuleDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ConfirmTopicRuleDestination where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ConfirmTopicRuleDestination where
  toPath ConfirmTopicRuleDestination' {..} =
    Lude.mconcat
      ["/confirmdestination/", Lude.toBS confirmationToken]

instance Lude.ToQuery ConfirmTopicRuleDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkConfirmTopicRuleDestinationResponse' smart constructor.
newtype ConfirmTopicRuleDestinationResponse = ConfirmTopicRuleDestinationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfirmTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkConfirmTopicRuleDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ConfirmTopicRuleDestinationResponse
mkConfirmTopicRuleDestinationResponse pResponseStatus_ =
  ConfirmTopicRuleDestinationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrdrsResponseStatus :: Lens.Lens' ConfirmTopicRuleDestinationResponse Lude.Int
ctrdrsResponseStatus = Lens.lens (responseStatus :: ConfirmTopicRuleDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ConfirmTopicRuleDestinationResponse)
{-# DEPRECATED ctrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
