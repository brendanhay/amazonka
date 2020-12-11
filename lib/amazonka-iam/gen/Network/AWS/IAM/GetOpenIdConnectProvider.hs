{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetOpenIdConnectProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified OpenID Connect (OIDC) provider resource object in IAM.
module Network.AWS.IAM.GetOpenIdConnectProvider
  ( -- * Creating a request
    GetOpenIdConnectProvider (..),
    mkGetOpenIdConnectProvider,

    -- ** Request lenses
    goicpOpenIdConnectProviderARN,

    -- * Destructuring the response
    GetOpenIdConnectProviderResponse (..),
    mkGetOpenIdConnectProviderResponse,

    -- ** Response lenses
    goicprsCreateDate,
    goicprsURL,
    goicprsThumbprintList,
    goicprsClientIdList,
    goicprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetOpenIdConnectProvider' smart constructor.
newtype GetOpenIdConnectProvider = GetOpenIdConnectProvider'
  { openIdConnectProviderARN ::
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

-- | Creates a value of 'GetOpenIdConnectProvider' with the minimum fields required to make a request.
--
-- * 'openIdConnectProviderARN' - The Amazon Resource Name (ARN) of the OIDC provider resource object in IAM to get information for. You can get a list of OIDC provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkGetOpenIdConnectProvider ::
  -- | 'openIdConnectProviderARN'
  Lude.Text ->
  GetOpenIdConnectProvider
mkGetOpenIdConnectProvider pOpenIdConnectProviderARN_ =
  GetOpenIdConnectProvider'
    { openIdConnectProviderARN =
        pOpenIdConnectProviderARN_
    }

-- | The Amazon Resource Name (ARN) of the OIDC provider resource object in IAM to get information for. You can get a list of OIDC provider resource ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'openIdConnectProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goicpOpenIdConnectProviderARN :: Lens.Lens' GetOpenIdConnectProvider Lude.Text
goicpOpenIdConnectProviderARN = Lens.lens (openIdConnectProviderARN :: GetOpenIdConnectProvider -> Lude.Text) (\s a -> s {openIdConnectProviderARN = a} :: GetOpenIdConnectProvider)
{-# DEPRECATED goicpOpenIdConnectProviderARN "Use generic-lens or generic-optics with 'openIdConnectProviderARN' instead." #-}

instance Lude.AWSRequest GetOpenIdConnectProvider where
  type Rs GetOpenIdConnectProvider = GetOpenIdConnectProviderResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetOpenIDConnectProviderResult"
      ( \s h x ->
          GetOpenIdConnectProviderResponse'
            Lude.<$> (x Lude..@? "CreateDate")
            Lude.<*> (x Lude..@? "Url")
            Lude.<*> ( x Lude..@? "ThumbprintList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "ClientIDList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOpenIdConnectProvider where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetOpenIdConnectProvider where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOpenIdConnectProvider where
  toQuery GetOpenIdConnectProvider' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetOpenIDConnectProvider" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "OpenIDConnectProviderArn" Lude.=: openIdConnectProviderARN
      ]

-- | Contains the response to a successful 'GetOpenIDConnectProvider' request.
--
-- /See:/ 'mkGetOpenIdConnectProviderResponse' smart constructor.
data GetOpenIdConnectProviderResponse = GetOpenIdConnectProviderResponse'
  { createDate ::
      Lude.Maybe Lude.ISO8601,
    url ::
      Lude.Maybe Lude.Text,
    thumbprintList ::
      Lude.Maybe [Lude.Text],
    clientIdList ::
      Lude.Maybe [Lude.Text],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOpenIdConnectProviderResponse' with the minimum fields required to make a request.
--
-- * 'clientIdList' - A list of client IDs (also known as audiences) that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
-- * 'createDate' - The date and time when the IAM OIDC provider resource object was created in the AWS account.
-- * 'responseStatus' - The response status code.
-- * 'thumbprintList' - A list of certificate thumbprints that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
-- * 'url' - The URL that the IAM OIDC provider resource object is associated with. For more information, see 'CreateOpenIDConnectProvider' .
mkGetOpenIdConnectProviderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOpenIdConnectProviderResponse
mkGetOpenIdConnectProviderResponse pResponseStatus_ =
  GetOpenIdConnectProviderResponse'
    { createDate = Lude.Nothing,
      url = Lude.Nothing,
      thumbprintList = Lude.Nothing,
      clientIdList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time when the IAM OIDC provider resource object was created in the AWS account.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goicprsCreateDate :: Lens.Lens' GetOpenIdConnectProviderResponse (Lude.Maybe Lude.ISO8601)
goicprsCreateDate = Lens.lens (createDate :: GetOpenIdConnectProviderResponse -> Lude.Maybe Lude.ISO8601) (\s a -> s {createDate = a} :: GetOpenIdConnectProviderResponse)
{-# DEPRECATED goicprsCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The URL that the IAM OIDC provider resource object is associated with. For more information, see 'CreateOpenIDConnectProvider' .
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goicprsURL :: Lens.Lens' GetOpenIdConnectProviderResponse (Lude.Maybe Lude.Text)
goicprsURL = Lens.lens (url :: GetOpenIdConnectProviderResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: GetOpenIdConnectProviderResponse)
{-# DEPRECATED goicprsURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | A list of certificate thumbprints that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
--
-- /Note:/ Consider using 'thumbprintList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goicprsThumbprintList :: Lens.Lens' GetOpenIdConnectProviderResponse (Lude.Maybe [Lude.Text])
goicprsThumbprintList = Lens.lens (thumbprintList :: GetOpenIdConnectProviderResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {thumbprintList = a} :: GetOpenIdConnectProviderResponse)
{-# DEPRECATED goicprsThumbprintList "Use generic-lens or generic-optics with 'thumbprintList' instead." #-}

-- | A list of client IDs (also known as audiences) that are associated with the specified IAM OIDC provider resource object. For more information, see 'CreateOpenIDConnectProvider' .
--
-- /Note:/ Consider using 'clientIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goicprsClientIdList :: Lens.Lens' GetOpenIdConnectProviderResponse (Lude.Maybe [Lude.Text])
goicprsClientIdList = Lens.lens (clientIdList :: GetOpenIdConnectProviderResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {clientIdList = a} :: GetOpenIdConnectProviderResponse)
{-# DEPRECATED goicprsClientIdList "Use generic-lens or generic-optics with 'clientIdList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goicprsResponseStatus :: Lens.Lens' GetOpenIdConnectProviderResponse Lude.Int
goicprsResponseStatus = Lens.lens (responseStatus :: GetOpenIdConnectProviderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOpenIdConnectProviderResponse)
{-# DEPRECATED goicprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
