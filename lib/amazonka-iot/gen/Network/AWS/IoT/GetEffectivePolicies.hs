{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetEffectivePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the policies that have an effect on the authorization behavior of the specified device when it connects to the AWS IoT device gateway.
module Network.AWS.IoT.GetEffectivePolicies
  ( -- * Creating a request
    GetEffectivePolicies (..),
    mkGetEffectivePolicies,

    -- ** Request lenses
    gepPrincipal,
    gepCognitoIdentityPoolId,
    gepThingName,

    -- * Destructuring the response
    GetEffectivePoliciesResponse (..),
    mkGetEffectivePoliciesResponse,

    -- ** Response lenses
    geprsEffectivePolicies,
    geprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetEffectivePolicies' smart constructor.
data GetEffectivePolicies = GetEffectivePolicies'
  { principal ::
      Lude.Maybe Lude.Text,
    cognitoIdentityPoolId :: Lude.Maybe Lude.Text,
    thingName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetEffectivePolicies' with the minimum fields required to make a request.
--
-- * 'cognitoIdentityPoolId' - The Cognito identity pool ID.
-- * 'principal' - The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
-- * 'thingName' - The thing name.
mkGetEffectivePolicies ::
  GetEffectivePolicies
mkGetEffectivePolicies =
  GetEffectivePolicies'
    { principal = Lude.Nothing,
      cognitoIdentityPoolId = Lude.Nothing,
      thingName = Lude.Nothing
    }

-- | The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepPrincipal :: Lens.Lens' GetEffectivePolicies (Lude.Maybe Lude.Text)
gepPrincipal = Lens.lens (principal :: GetEffectivePolicies -> Lude.Maybe Lude.Text) (\s a -> s {principal = a} :: GetEffectivePolicies)
{-# DEPRECATED gepPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | The Cognito identity pool ID.
--
-- /Note:/ Consider using 'cognitoIdentityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepCognitoIdentityPoolId :: Lens.Lens' GetEffectivePolicies (Lude.Maybe Lude.Text)
gepCognitoIdentityPoolId = Lens.lens (cognitoIdentityPoolId :: GetEffectivePolicies -> Lude.Maybe Lude.Text) (\s a -> s {cognitoIdentityPoolId = a} :: GetEffectivePolicies)
{-# DEPRECATED gepCognitoIdentityPoolId "Use generic-lens or generic-optics with 'cognitoIdentityPoolId' instead." #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepThingName :: Lens.Lens' GetEffectivePolicies (Lude.Maybe Lude.Text)
gepThingName = Lens.lens (thingName :: GetEffectivePolicies -> Lude.Maybe Lude.Text) (\s a -> s {thingName = a} :: GetEffectivePolicies)
{-# DEPRECATED gepThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest GetEffectivePolicies where
  type Rs GetEffectivePolicies = GetEffectivePoliciesResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetEffectivePoliciesResponse'
            Lude.<$> (x Lude..?> "effectivePolicies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetEffectivePolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetEffectivePolicies where
  toJSON GetEffectivePolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("principal" Lude..=) Lude.<$> principal,
            ("cognitoIdentityPoolId" Lude..=) Lude.<$> cognitoIdentityPoolId
          ]
      )

instance Lude.ToPath GetEffectivePolicies where
  toPath = Lude.const "/effective-policies"

instance Lude.ToQuery GetEffectivePolicies where
  toQuery GetEffectivePolicies' {..} =
    Lude.mconcat ["thingName" Lude.=: thingName]

-- | /See:/ 'mkGetEffectivePoliciesResponse' smart constructor.
data GetEffectivePoliciesResponse = GetEffectivePoliciesResponse'
  { effectivePolicies ::
      Lude.Maybe [EffectivePolicy],
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

-- | Creates a value of 'GetEffectivePoliciesResponse' with the minimum fields required to make a request.
--
-- * 'effectivePolicies' - The effective policies.
-- * 'responseStatus' - The response status code.
mkGetEffectivePoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetEffectivePoliciesResponse
mkGetEffectivePoliciesResponse pResponseStatus_ =
  GetEffectivePoliciesResponse'
    { effectivePolicies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The effective policies.
--
-- /Note:/ Consider using 'effectivePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprsEffectivePolicies :: Lens.Lens' GetEffectivePoliciesResponse (Lude.Maybe [EffectivePolicy])
geprsEffectivePolicies = Lens.lens (effectivePolicies :: GetEffectivePoliciesResponse -> Lude.Maybe [EffectivePolicy]) (\s a -> s {effectivePolicies = a} :: GetEffectivePoliciesResponse)
{-# DEPRECATED geprsEffectivePolicies "Use generic-lens or generic-optics with 'effectivePolicies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprsResponseStatus :: Lens.Lens' GetEffectivePoliciesResponse Lude.Int
geprsResponseStatus = Lens.lens (responseStatus :: GetEffectivePoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetEffectivePoliciesResponse)
{-# DEPRECATED geprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
