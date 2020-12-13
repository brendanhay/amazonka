{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.CreatePartnerEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Called by an SaaS partner to create a partner event source. This operation is not used by AWS customers.
--
-- Each partner event source can be used by one AWS account to create a matching partner event bus in that AWS account. A SaaS partner must create one partner event source for each AWS account that wants to receive those event types.
-- A partner event source creates events based on resources within the SaaS partner's service or application.
-- An AWS account that creates a partner event bus that matches the partner event source can use that event bus to receive events from the partner, and then process them using AWS Events rules and targets.
-- Partner event source names follow this format:
-- @/partner_name/ //event_namespace/ //event_name/ @
-- /partner_name/ is determined during partner registration and identifies the partner to AWS customers. /event_namespace/ is determined by the partner and is a way for the partner to categorize their events. /event_name/ is determined by the partner, and should uniquely identify an event-generating resource within the partner system. The combination of /event_namespace/ and /event_name/ should help AWS customers decide whether to create an event bus to receive these events.
module Network.AWS.CloudWatchEvents.CreatePartnerEventSource
  ( -- * Creating a request
    CreatePartnerEventSource (..),
    mkCreatePartnerEventSource,

    -- ** Request lenses
    cpesAccount,
    cpesName,

    -- * Destructuring the response
    CreatePartnerEventSourceResponse (..),
    mkCreatePartnerEventSourceResponse,

    -- ** Response lenses
    cpesrsEventSourceARN,
    cpesrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreatePartnerEventSource' smart constructor.
data CreatePartnerEventSource = CreatePartnerEventSource'
  { -- | The AWS account ID that is permitted to create a matching partner event bus for this partner event source.
    account :: Lude.Text,
    -- | The name of the partner event source. This name must be unique and must be in the format @/partner_name/ //event_namespace/ //event_name/ @ . The AWS account that wants to use this partner event source must create a partner event bus with a name that matches the name of the partner event source.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePartnerEventSource' with the minimum fields required to make a request.
--
-- * 'account' - The AWS account ID that is permitted to create a matching partner event bus for this partner event source.
-- * 'name' - The name of the partner event source. This name must be unique and must be in the format @/partner_name/ //event_namespace/ //event_name/ @ . The AWS account that wants to use this partner event source must create a partner event bus with a name that matches the name of the partner event source.
mkCreatePartnerEventSource ::
  -- | 'account'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreatePartnerEventSource
mkCreatePartnerEventSource pAccount_ pName_ =
  CreatePartnerEventSource' {account = pAccount_, name = pName_}

-- | The AWS account ID that is permitted to create a matching partner event bus for this partner event source.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpesAccount :: Lens.Lens' CreatePartnerEventSource Lude.Text
cpesAccount = Lens.lens (account :: CreatePartnerEventSource -> Lude.Text) (\s a -> s {account = a} :: CreatePartnerEventSource)
{-# DEPRECATED cpesAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The name of the partner event source. This name must be unique and must be in the format @/partner_name/ //event_namespace/ //event_name/ @ . The AWS account that wants to use this partner event source must create a partner event bus with a name that matches the name of the partner event source.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpesName :: Lens.Lens' CreatePartnerEventSource Lude.Text
cpesName = Lens.lens (name :: CreatePartnerEventSource -> Lude.Text) (\s a -> s {name = a} :: CreatePartnerEventSource)
{-# DEPRECATED cpesName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreatePartnerEventSource where
  type Rs CreatePartnerEventSource = CreatePartnerEventSourceResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePartnerEventSourceResponse'
            Lude.<$> (x Lude..?> "EventSourceArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePartnerEventSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.CreatePartnerEventSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePartnerEventSource where
  toJSON CreatePartnerEventSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Account" Lude..= account),
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreatePartnerEventSource where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePartnerEventSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePartnerEventSourceResponse' smart constructor.
data CreatePartnerEventSourceResponse = CreatePartnerEventSourceResponse'
  { -- | The ARN of the partner event source.
    eventSourceARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePartnerEventSourceResponse' with the minimum fields required to make a request.
--
-- * 'eventSourceARN' - The ARN of the partner event source.
-- * 'responseStatus' - The response status code.
mkCreatePartnerEventSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePartnerEventSourceResponse
mkCreatePartnerEventSourceResponse pResponseStatus_ =
  CreatePartnerEventSourceResponse'
    { eventSourceARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the partner event source.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpesrsEventSourceARN :: Lens.Lens' CreatePartnerEventSourceResponse (Lude.Maybe Lude.Text)
cpesrsEventSourceARN = Lens.lens (eventSourceARN :: CreatePartnerEventSourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: CreatePartnerEventSourceResponse)
{-# DEPRECATED cpesrsEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpesrsResponseStatus :: Lens.Lens' CreatePartnerEventSourceResponse Lude.Int
cpesrsResponseStatus = Lens.lens (responseStatus :: CreatePartnerEventSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePartnerEventSourceResponse)
{-# DEPRECATED cpesrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
