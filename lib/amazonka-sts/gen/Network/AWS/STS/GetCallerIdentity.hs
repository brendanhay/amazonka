{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.GetCallerIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the IAM user or role whose credentials are used to call the operation.
module Network.AWS.STS.GetCallerIdentity
  ( -- * Creating a request
    GetCallerIdentity (..),
    mkGetCallerIdentity,

    -- * Destructuring the response
    GetCallerIdentityResponse (..),
    mkGetCallerIdentityResponse,

    -- ** Response lenses
    gcirsARN,
    gcirsAccount,
    gcirsUserId,
    gcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.STS.Types

-- | /See:/ 'mkGetCallerIdentity' smart constructor.
data GetCallerIdentity = GetCallerIdentity'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCallerIdentity' with the minimum fields required to make a request.
mkGetCallerIdentity ::
  GetCallerIdentity
mkGetCallerIdentity = GetCallerIdentity'

instance Lude.AWSRequest GetCallerIdentity where
  type Rs GetCallerIdentity = GetCallerIdentityResponse
  request = Req.postQuery stsService
  response =
    Res.receiveXMLWrapper
      "GetCallerIdentityResult"
      ( \s h x ->
          GetCallerIdentityResponse'
            Lude.<$> (x Lude..@? "Arn")
            Lude.<*> (x Lude..@? "Account")
            Lude.<*> (x Lude..@? "UserId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCallerIdentity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCallerIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCallerIdentity where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("GetCallerIdentity" :: Lude.ByteString),
            "Version" Lude.=: ("2011-06-15" :: Lude.ByteString)
          ]
      )

-- | Contains the response to a successful 'GetCallerIdentity' request, including information about the entity making the request.
--
-- /See:/ 'mkGetCallerIdentityResponse' smart constructor.
data GetCallerIdentityResponse = GetCallerIdentityResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    account :: Lude.Maybe Lude.Text,
    userId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetCallerIdentityResponse' with the minimum fields required to make a request.
--
-- * 'account' - The AWS account ID number of the account that owns or contains the calling entity.
-- * 'arn' - The AWS ARN associated with the calling entity.
-- * 'responseStatus' - The response status code.
-- * 'userId' - The unique identifier of the calling entity. The exact value depends on the type of entity that is making the call. The values returned are those listed in the __aws:userid__ column in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table> found on the __Policy Variables__ reference page in the /IAM User Guide/ .
mkGetCallerIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCallerIdentityResponse
mkGetCallerIdentityResponse pResponseStatus_ =
  GetCallerIdentityResponse'
    { arn = Lude.Nothing,
      account = Lude.Nothing,
      userId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The AWS ARN associated with the calling entity.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsARN :: Lens.Lens' GetCallerIdentityResponse (Lude.Maybe Lude.Text)
gcirsARN = Lens.lens (arn :: GetCallerIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetCallerIdentityResponse)
{-# DEPRECATED gcirsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The AWS account ID number of the account that owns or contains the calling entity.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsAccount :: Lens.Lens' GetCallerIdentityResponse (Lude.Maybe Lude.Text)
gcirsAccount = Lens.lens (account :: GetCallerIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: GetCallerIdentityResponse)
{-# DEPRECATED gcirsAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The unique identifier of the calling entity. The exact value depends on the type of entity that is making the call. The values returned are those listed in the __aws:userid__ column in the <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_variables.html#principaltable Principal table> found on the __Policy Variables__ reference page in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsUserId :: Lens.Lens' GetCallerIdentityResponse (Lude.Maybe Lude.Text)
gcirsUserId = Lens.lens (userId :: GetCallerIdentityResponse -> Lude.Maybe Lude.Text) (\s a -> s {userId = a} :: GetCallerIdentityResponse)
{-# DEPRECATED gcirsUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcirsResponseStatus :: Lens.Lens' GetCallerIdentityResponse Lude.Int
gcirsResponseStatus = Lens.lens (responseStatus :: GetCallerIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCallerIdentityResponse)
{-# DEPRECATED gcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
