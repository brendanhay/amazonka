{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateOpenIdConnectProviderThumbprint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the existing list of server certificate thumbprints associated with an OpenID Connect (OIDC) provider resource object with a new list of thumbprints.
--
-- The list that you pass with this operation completely replaces the existing list of thumbprints. (The lists are not merged.)
-- Typically, you need to update a thumbprint only when the identity provider's certificate changes, which occurs rarely. However, if the provider's certificate /does/ change, any attempt to assume an IAM role that specifies the OIDC provider as a principal fails until the certificate thumbprint is updated.
module Network.AWS.IAM.UpdateOpenIdConnectProviderThumbprint
  ( -- * Creating a request
    UpdateOpenIdConnectProviderThumbprint (..),
    mkUpdateOpenIdConnectProviderThumbprint,

    -- ** Request lenses
    uoicptThumbprintList,
    uoicptOpenIdConnectProviderARN,

    -- * Destructuring the response
    UpdateOpenIdConnectProviderThumbprintResponse (..),
    mkUpdateOpenIdConnectProviderThumbprintResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateOpenIdConnectProviderThumbprint' smart constructor.
data UpdateOpenIdConnectProviderThumbprint = UpdateOpenIdConnectProviderThumbprint'
  { -- | A list of certificate thumbprints that are associated with the specified IAM OpenID Connect provider. For more information, see 'CreateOpenIDConnectProvider' .
    thumbprintList :: [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource object for which you want to update the thumbprint. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    openIdConnectProviderARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOpenIdConnectProviderThumbprint' with the minimum fields required to make a request.
--
-- * 'thumbprintList' - A list of certificate thumbprints that are associated with the specified IAM OpenID Connect provider. For more information, see 'CreateOpenIDConnectProvider' .
-- * 'openIdConnectProviderARN' - The Amazon Resource Name (ARN) of the IAM OIDC provider resource object for which you want to update the thumbprint. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkUpdateOpenIdConnectProviderThumbprint ::
  -- | 'openIdConnectProviderARN'
  Lude.Text ->
  UpdateOpenIdConnectProviderThumbprint
mkUpdateOpenIdConnectProviderThumbprint pOpenIdConnectProviderARN_ =
  UpdateOpenIdConnectProviderThumbprint'
    { thumbprintList =
        Lude.mempty,
      openIdConnectProviderARN = pOpenIdConnectProviderARN_
    }

-- | A list of certificate thumbprints that are associated with the specified IAM OpenID Connect provider. For more information, see 'CreateOpenIDConnectProvider' .
--
-- /Note:/ Consider using 'thumbprintList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoicptThumbprintList :: Lens.Lens' UpdateOpenIdConnectProviderThumbprint [Lude.Text]
uoicptThumbprintList = Lens.lens (thumbprintList :: UpdateOpenIdConnectProviderThumbprint -> [Lude.Text]) (\s a -> s {thumbprintList = a} :: UpdateOpenIdConnectProviderThumbprint)
{-# DEPRECATED uoicptThumbprintList "Use generic-lens or generic-optics with 'thumbprintList' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM OIDC provider resource object for which you want to update the thumbprint. You can get a list of OIDC provider ARNs by using the 'ListOpenIDConnectProviders' operation.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'openIdConnectProviderARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoicptOpenIdConnectProviderARN :: Lens.Lens' UpdateOpenIdConnectProviderThumbprint Lude.Text
uoicptOpenIdConnectProviderARN = Lens.lens (openIdConnectProviderARN :: UpdateOpenIdConnectProviderThumbprint -> Lude.Text) (\s a -> s {openIdConnectProviderARN = a} :: UpdateOpenIdConnectProviderThumbprint)
{-# DEPRECATED uoicptOpenIdConnectProviderARN "Use generic-lens or generic-optics with 'openIdConnectProviderARN' instead." #-}

instance Lude.AWSRequest UpdateOpenIdConnectProviderThumbprint where
  type
    Rs UpdateOpenIdConnectProviderThumbprint =
      UpdateOpenIdConnectProviderThumbprintResponse
  request = Req.postQuery iamService
  response =
    Res.receiveNull UpdateOpenIdConnectProviderThumbprintResponse'

instance Lude.ToHeaders UpdateOpenIdConnectProviderThumbprint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateOpenIdConnectProviderThumbprint where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateOpenIdConnectProviderThumbprint where
  toQuery UpdateOpenIdConnectProviderThumbprint' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("UpdateOpenIDConnectProviderThumbprint" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "ThumbprintList" Lude.=: Lude.toQueryList "member" thumbprintList,
        "OpenIDConnectProviderArn" Lude.=: openIdConnectProviderARN
      ]

-- | /See:/ 'mkUpdateOpenIdConnectProviderThumbprintResponse' smart constructor.
data UpdateOpenIdConnectProviderThumbprintResponse = UpdateOpenIdConnectProviderThumbprintResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateOpenIdConnectProviderThumbprintResponse' with the minimum fields required to make a request.
mkUpdateOpenIdConnectProviderThumbprintResponse ::
  UpdateOpenIdConnectProviderThumbprintResponse
mkUpdateOpenIdConnectProviderThumbprintResponse =
  UpdateOpenIdConnectProviderThumbprintResponse'
