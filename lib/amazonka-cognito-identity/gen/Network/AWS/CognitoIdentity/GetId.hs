{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates (or retrieves) a Cognito ID. Supplying multiple logins will create an implicit linked account.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.GetId
  ( -- * Creating a request
    GetId (..),
    mkGetId,

    -- ** Request lenses
    giIdentityPoolId,
    giAccountId,
    giLogins,

    -- * Destructuring the response
    GetIdResponse (..),
    mkGetIdResponse,

    -- ** Response lenses
    girsIdentityId,
    girsResponseStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the GetId action.
--
-- /See:/ 'mkGetId' smart constructor.
data GetId = GetId'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Lude.Text,
    -- | A standard AWS account ID (9+ digits).
    accountId :: Lude.Maybe Lude.Text,
    -- | A set of optional name-value pairs that map provider names to provider tokens. The available provider names for @Logins@ are as follows:
    --
    --
    --     * Facebook: @graph.facebook.com@
    --
    --
    --     * Amazon Cognito user pool: @cognito-idp.<region>.amazonaws.com/<YOUR_USER_POOL_ID>@ , for example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
    --
    --
    --     * Google: @accounts.google.com@
    --
    --
    --     * Amazon: @www.amazon.com@
    --
    --
    --     * Twitter: @api.twitter.com@
    --
    --
    --     * Digits: @www.digits.com@
    logins :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetId' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'accountId' - A standard AWS account ID (9+ digits).
-- * 'logins' - A set of optional name-value pairs that map provider names to provider tokens. The available provider names for @Logins@ are as follows:
--
--
--     * Facebook: @graph.facebook.com@
--
--
--     * Amazon Cognito user pool: @cognito-idp.<region>.amazonaws.com/<YOUR_USER_POOL_ID>@ , for example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
--
--
--     * Google: @accounts.google.com@
--
--
--     * Amazon: @www.amazon.com@
--
--
--     * Twitter: @api.twitter.com@
--
--
--     * Digits: @www.digits.com@
mkGetId ::
  -- | 'identityPoolId'
  Lude.Text ->
  GetId
mkGetId pIdentityPoolId_ =
  GetId'
    { identityPoolId = pIdentityPoolId_,
      accountId = Lude.Nothing,
      logins = Lude.Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giIdentityPoolId :: Lens.Lens' GetId Lude.Text
giIdentityPoolId = Lens.lens (identityPoolId :: GetId -> Lude.Text) (\s a -> s {identityPoolId = a} :: GetId)
{-# DEPRECATED giIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A standard AWS account ID (9+ digits).
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giAccountId :: Lens.Lens' GetId (Lude.Maybe Lude.Text)
giAccountId = Lens.lens (accountId :: GetId -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: GetId)
{-# DEPRECATED giAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | A set of optional name-value pairs that map provider names to provider tokens. The available provider names for @Logins@ are as follows:
--
--
--     * Facebook: @graph.facebook.com@
--
--
--     * Amazon Cognito user pool: @cognito-idp.<region>.amazonaws.com/<YOUR_USER_POOL_ID>@ , for example, @cognito-idp.us-east-1.amazonaws.com/us-east-1_123456789@ .
--
--
--     * Google: @accounts.google.com@
--
--
--     * Amazon: @www.amazon.com@
--
--
--     * Twitter: @api.twitter.com@
--
--
--     * Digits: @www.digits.com@
--
--
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giLogins :: Lens.Lens' GetId (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
giLogins = Lens.lens (logins :: GetId -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {logins = a} :: GetId)
{-# DEPRECATED giLogins "Use generic-lens or generic-optics with 'logins' instead." #-}

instance Lude.AWSRequest GetId where
  type Rs GetId = GetIdResponse
  request = Req.postJSON cognitoIdentityService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIdResponse'
            Lude.<$> (x Lude..?> "IdentityId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetId where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSCognitoIdentityService.GetId" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetId where
  toJSON GetId' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IdentityPoolId" Lude..= identityPoolId),
            ("AccountId" Lude..=) Lude.<$> accountId,
            ("Logins" Lude..=) Lude.<$> logins
          ]
      )

instance Lude.ToPath GetId where
  toPath = Lude.const "/"

instance Lude.ToQuery GetId where
  toQuery = Lude.const Lude.mempty

-- | Returned in response to a GetId request.
--
-- /See:/ 'mkGetIdResponse' smart constructor.
data GetIdResponse = GetIdResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdResponse' with the minimum fields required to make a request.
--
-- * 'identityId' - A unique identifier in the format REGION:GUID.
-- * 'responseStatus' - The response status code.
mkGetIdResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIdResponse
mkGetIdResponse pResponseStatus_ =
  GetIdResponse'
    { identityId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsIdentityId :: Lens.Lens' GetIdResponse (Lude.Maybe Lude.Text)
girsIdentityId = Lens.lens (identityId :: GetIdResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityId = a} :: GetIdResponse)
{-# DEPRECATED girsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girsResponseStatus :: Lens.Lens' GetIdResponse Lude.Int
girsResponseStatus = Lens.lens (responseStatus :: GetIdResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdResponse)
{-# DEPRECATED girsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
