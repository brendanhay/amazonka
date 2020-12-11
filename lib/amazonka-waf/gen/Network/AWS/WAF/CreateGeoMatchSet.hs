{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.CreateGeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an 'GeoMatchSet' , which you use to specify which web requests you want to allow or block based on the country that the requests originate from. For example, if you're receiving a lot of requests from one or more countries and you want to block the requests, you can create an @GeoMatchSet@ that contains those countries and then configure AWS WAF to block the requests.
--
-- To create and configure a @GeoMatchSet@ , perform the following steps:
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateGeoMatchSet@ request.
--
--
--     * Submit a @CreateGeoMatchSet@ request.
--
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateGeoMatchSet' request.
--
--
--     * Submit an @UpdateGeoMatchSetSet@ request to specify the countries that you want AWS WAF to watch for.
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateGeoMatchSet
  ( -- * Creating a request
    CreateGeoMatchSet (..),
    mkCreateGeoMatchSet,

    -- ** Request lenses
    cgmsName,
    cgmsChangeToken,

    -- * Destructuring the response
    CreateGeoMatchSetResponse (..),
    mkCreateGeoMatchSetResponse,

    -- ** Response lenses
    cgmsrsGeoMatchSet,
    cgmsrsChangeToken,
    cgmsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkCreateGeoMatchSet' smart constructor.
data CreateGeoMatchSet = CreateGeoMatchSet'
  { name :: Lude.Text,
    changeToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGeoMatchSet' with the minimum fields required to make a request.
--
-- * 'changeToken' - The value returned by the most recent call to 'GetChangeToken' .
-- * 'name' - A friendly name or description of the 'GeoMatchSet' . You can't change @Name@ after you create the @GeoMatchSet@ .
mkCreateGeoMatchSet ::
  -- | 'name'
  Lude.Text ->
  -- | 'changeToken'
  Lude.Text ->
  CreateGeoMatchSet
mkCreateGeoMatchSet pName_ pChangeToken_ =
  CreateGeoMatchSet' {name = pName_, changeToken = pChangeToken_}

-- | A friendly name or description of the 'GeoMatchSet' . You can't change @Name@ after you create the @GeoMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsName :: Lens.Lens' CreateGeoMatchSet Lude.Text
cgmsName = Lens.lens (name :: CreateGeoMatchSet -> Lude.Text) (\s a -> s {name = a} :: CreateGeoMatchSet)
{-# DEPRECATED cgmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsChangeToken :: Lens.Lens' CreateGeoMatchSet Lude.Text
cgmsChangeToken = Lens.lens (changeToken :: CreateGeoMatchSet -> Lude.Text) (\s a -> s {changeToken = a} :: CreateGeoMatchSet)
{-# DEPRECATED cgmsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

instance Lude.AWSRequest CreateGeoMatchSet where
  type Rs CreateGeoMatchSet = CreateGeoMatchSetResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGeoMatchSetResponse'
            Lude.<$> (x Lude..?> "GeoMatchSet")
            Lude.<*> (x Lude..?> "ChangeToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGeoMatchSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.CreateGeoMatchSet" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGeoMatchSet where
  toJSON CreateGeoMatchSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("ChangeToken" Lude..= changeToken)
          ]
      )

instance Lude.ToPath CreateGeoMatchSet where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGeoMatchSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGeoMatchSetResponse' smart constructor.
data CreateGeoMatchSetResponse = CreateGeoMatchSetResponse'
  { geoMatchSet ::
      Lude.Maybe GeoMatchSet,
    changeToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGeoMatchSetResponse' with the minimum fields required to make a request.
--
-- * 'changeToken' - The @ChangeToken@ that you used to submit the @CreateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
-- * 'geoMatchSet' - The 'GeoMatchSet' returned in the @CreateGeoMatchSet@ response. The @GeoMatchSet@ contains no @GeoMatchConstraints@ .
-- * 'responseStatus' - The response status code.
mkCreateGeoMatchSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGeoMatchSetResponse
mkCreateGeoMatchSetResponse pResponseStatus_ =
  CreateGeoMatchSetResponse'
    { geoMatchSet = Lude.Nothing,
      changeToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'GeoMatchSet' returned in the @CreateGeoMatchSet@ response. The @GeoMatchSet@ contains no @GeoMatchConstraints@ .
--
-- /Note:/ Consider using 'geoMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsrsGeoMatchSet :: Lens.Lens' CreateGeoMatchSetResponse (Lude.Maybe GeoMatchSet)
cgmsrsGeoMatchSet = Lens.lens (geoMatchSet :: CreateGeoMatchSetResponse -> Lude.Maybe GeoMatchSet) (\s a -> s {geoMatchSet = a} :: CreateGeoMatchSetResponse)
{-# DEPRECATED cgmsrsGeoMatchSet "Use generic-lens or generic-optics with 'geoMatchSet' instead." #-}

-- | The @ChangeToken@ that you used to submit the @CreateGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsrsChangeToken :: Lens.Lens' CreateGeoMatchSetResponse (Lude.Maybe Lude.Text)
cgmsrsChangeToken = Lens.lens (changeToken :: CreateGeoMatchSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeToken = a} :: CreateGeoMatchSetResponse)
{-# DEPRECATED cgmsrsChangeToken "Use generic-lens or generic-optics with 'changeToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgmsrsResponseStatus :: Lens.Lens' CreateGeoMatchSetResponse Lude.Int
cgmsrsResponseStatus = Lens.lens (responseStatus :: CreateGeoMatchSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGeoMatchSetResponse)
{-# DEPRECATED cgmsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
