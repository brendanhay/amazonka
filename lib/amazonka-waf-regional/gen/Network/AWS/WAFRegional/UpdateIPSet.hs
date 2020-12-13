{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.UpdateIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'IPSetDescriptor' objects in an @IPSet@ . For each @IPSetDescriptor@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change an @IPSetDescriptor@ object, you delete the existing object and add a new one.
--
--
--     * The IP address version, @IPv4@ or @IPv6@ .
--
--
--     * The IP address in CIDR notation, for example, @192.0.2.0/24@ (for the range of IP addresses from @192.0.2.0@ to @192.0.2.255@ ) or @192.0.2.44/32@ (for the individual IP address @192.0.2.44@ ).
--
--
-- AWS WAF supports IPv4 address ranges: /8 and any range between /16 through /32. AWS WAF supports IPv6 address ranges: /24, /32, /48, /56, /64, and /128. For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
-- IPv6 addresses can be represented using any of the following formats:
--
--     * 1111:0000:0000:0000:0000:0000:0000:0111/128
--
--
--     * 1111:0:0:0:0:0:0:0111/128
--
--
--     * 1111::0111/128
--
--
--     * 1111::111/128
--
--
-- You use an @IPSet@ to specify which web requests you want to allow or block based on the IP addresses that the requests originated from. For example, if you're receiving a lot of requests from one or a small number of IP addresses and you want to block the requests, you can create an @IPSet@ that specifies those IP addresses, and then configure AWS WAF to block the requests.
-- To create and configure an @IPSet@ , perform the following steps:
--
--     * Submit a 'CreateIPSet' request.
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--
--     * Submit an @UpdateIPSet@ request to specify the IP addresses that you want AWS WAF to watch for.
--
--
-- When you update an @IPSet@ , you specify the IP addresses that you want to add and/or the IP addresses that you want to delete. If you want to change an IP address, you delete the existing IP address and add the new one.
-- You can insert a maximum of 1000 addresses in a single request.
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAFRegional.UpdateIPSet
  ( -- * Creating a request
    UpdateIPSet (..),
    mkUpdateIPSet,

    -- ** Request lenses
    uisUpdates,
    uisChangeToken,
    uisIPSetId,

    -- * Destructuring the response
    UpdateIPSetResponse (..),
    mkUpdateIPSetResponse,

    -- ** Response lenses
    uisrsChangeToken,
    uisrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkUpdateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { -- | An array of @IPSetUpdate@ objects that you want to insert into or delete from an 'IPSet' . For more information, see the applicable data types:
    --
    --
    --     * 'IPSetUpdate' : Contains @Action@ and @IPSetDescriptor@
    --
    --
    --     * 'IPSetDescriptor' : Contains @Type@ and @Value@
    --
    --
    -- You can insert a maximum of 1000 addresses in a single request.
    updates :: Lude.NonEmpty IPSetUpdate,
    -- | The value returned by the most recent call to 'GetChangeToken' .
    changeToken :: Lude.Text,
    -- | The @IPSetId@ of the 'IPSet' that you want to update. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
    ipSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIPSet' with the minimum fields required to make a request.
--
-- * 'updates' - An array of @IPSetUpdate@ objects that you want to insert into or delete from an 'IPSet' . For more information, see the applicable data types:
--
--
--     * 'IPSetUpdate' : Contains @Action@ and @IPSetDescriptor@
--
--
--     * 'IPSetDescriptor' : Contains @Type@ and @Value@
--
--
-- You can insert a maximum of 1000 addresses in a single request.
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'ipSetId' - The @IPSetId@ of the 'IPSet' that you want to update. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
mkUpdateIPSet ::
  -- | 'updates'
  Lude.NonEmpty IPSetUpdate ->
  -- | 'changeToken'
  Lude.Text ->
  -- | 'ipSetId'
  Lude.Text ->
  UpdateIPSet
mkUpdateIPSet pUpdates_ pChangeToken_ pIPSetId_ =
  UpdateIPSet'
    { updates = pUpdates_,
      changeToken = pChangeToken_,
      ipSetId = pIPSetId_
    }

-- | An array of @IPSetUpdate@ objects that you want to insert into or delete from an 'IPSet' . For more information, see the applicable data types:
--
--
--     * 'IPSetUpdate' : Contains @Action@ and @IPSetDescriptor@
--
--
--     * 'IPSetDescriptor' : Contains @Type@ and @Value@
--
--
-- You can insert a maximum of 1000 addresses in a single request.
--
-- /Note:/ Consider using 'updates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisUpdates :: Lens.Lens' UpdateIPSet (Lude.NonEmpty IPSetUpdate)
uisUpdates = Lens.lens (updates :: UpdateIPSet -> Lude.NonEmpty IPSetUpdate) (\s a -> s {updates = a} :: UpdateIPSet)
{-# DEPRECATED uisUpdates "Use generic-lens or generic-optics with 'updates' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisChangeToken :: Lens.Lens' UpdateIPSet Lude.Text
uisChangeToken = Lens.lens (changeToken :: UpdateIPSet -> Lude.Text) (\s a -> s {changeToken = a} :: UpdateIPSet)
{-# DEPRECATED uisChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The @IPSetId@ of the 'IPSet' that you want to update. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisIPSetId :: Lens.Lens' UpdateIPSet Lude.Text
uisIPSetId = Lens.lens (ipSetId :: UpdateIPSet -> Lude.Text) (\s a -> s {ipSetId = a} :: UpdateIPSet)
{-# DEPRECATED uisIPSetId "Use generic-lens or generic-optics with 'ipSetId' instead." #-}

instance Lude.AWSRequest UpdateIPSet where
  type Rs UpdateIPSet = UpdateIPSetResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateIPSetResponse'
            Lude.<$> (x Lude..?> "ChangeToken") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateIPSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_Regional_20161128.UpdateIPSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateIPSet where
  toJSON UpdateIPSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Updates" Lude..= updates),
            Lude.Just ("ChangeToken" Lude..= changeToken),
            Lude.Just ("IPSetId" Lude..= ipSetId)
          ]
      )

instance Lude.ToPath UpdateIPSet where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateIPSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateIPSetResponse' smart constructor.
data UpdateIPSetResponse = UpdateIPSetResponse'
  { -- | The @ChangeToken@ that you used to submit the @UpdateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
    changeToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIPSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @UpdateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'responseStatus' - The response status code.
mkUpdateIPSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateIPSetResponse
mkUpdateIPSetResponse pResponseStatus_ =
  UpdateIPSetResponse'
    { changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @UpdateIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisrsChangeToken :: Lens.Lens' UpdateIPSetResponse (Lude.Maybe Lude.Text)
uisrsChangeToken = Lens.lens (changeToken :: UpdateIPSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: UpdateIPSetResponse)
{-# DEPRECATED uisrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uisrsResponseStatus :: Lens.Lens' UpdateIPSetResponse Lude.Int
uisrsResponseStatus = Lens.lens (responseStatus :: UpdateIPSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateIPSetResponse)
{-# DEPRECATED uisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
