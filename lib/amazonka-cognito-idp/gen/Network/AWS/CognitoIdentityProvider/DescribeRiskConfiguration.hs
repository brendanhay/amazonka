{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the risk configuration.
module Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
  ( -- * Creating a request
    DescribeRiskConfiguration (..),
    mkDescribeRiskConfiguration,

    -- ** Request lenses
    drcClientId,
    drcUserPoolId,

    -- * Destructuring the response
    DescribeRiskConfigurationResponse (..),
    mkDescribeRiskConfigurationResponse,

    -- ** Response lenses
    drcrsRiskConfiguration,
    drcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRiskConfiguration' smart constructor.
data DescribeRiskConfiguration = DescribeRiskConfiguration'
  { -- | The app client ID.
    clientId :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The user pool ID.
    userPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRiskConfiguration' with the minimum fields required to make a request.
--
-- * 'clientId' - The app client ID.
-- * 'userPoolId' - The user pool ID.
mkDescribeRiskConfiguration ::
  -- | 'userPoolId'
  Lude.Text ->
  DescribeRiskConfiguration
mkDescribeRiskConfiguration pUserPoolId_ =
  DescribeRiskConfiguration'
    { clientId = Lude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The app client ID.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcClientId :: Lens.Lens' DescribeRiskConfiguration (Lude.Maybe (Lude.Sensitive Lude.Text))
drcClientId = Lens.lens (clientId :: DescribeRiskConfiguration -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {clientId = a} :: DescribeRiskConfiguration)
{-# DEPRECATED drcClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcUserPoolId :: Lens.Lens' DescribeRiskConfiguration Lude.Text
drcUserPoolId = Lens.lens (userPoolId :: DescribeRiskConfiguration -> Lude.Text) (\s a -> s {userPoolId = a} :: DescribeRiskConfiguration)
{-# DEPRECATED drcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest DescribeRiskConfiguration where
  type
    Rs DescribeRiskConfiguration =
      DescribeRiskConfigurationResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRiskConfigurationResponse'
            Lude.<$> (x Lude..:> "RiskConfiguration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRiskConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.DescribeRiskConfiguration" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRiskConfiguration where
  toJSON DescribeRiskConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientId" Lude..=) Lude.<$> clientId,
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath DescribeRiskConfiguration where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRiskConfiguration where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeRiskConfigurationResponse' smart constructor.
data DescribeRiskConfigurationResponse = DescribeRiskConfigurationResponse'
  { -- | The risk configuration.
    riskConfiguration :: RiskConfigurationType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRiskConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'riskConfiguration' - The risk configuration.
-- * 'responseStatus' - The response status code.
mkDescribeRiskConfigurationResponse ::
  -- | 'riskConfiguration'
  RiskConfigurationType ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRiskConfigurationResponse
mkDescribeRiskConfigurationResponse
  pRiskConfiguration_
  pResponseStatus_ =
    DescribeRiskConfigurationResponse'
      { riskConfiguration =
          pRiskConfiguration_,
        responseStatus = pResponseStatus_
      }

-- | The risk configuration.
--
-- /Note:/ Consider using 'riskConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsRiskConfiguration :: Lens.Lens' DescribeRiskConfigurationResponse RiskConfigurationType
drcrsRiskConfiguration = Lens.lens (riskConfiguration :: DescribeRiskConfigurationResponse -> RiskConfigurationType) (\s a -> s {riskConfiguration = a} :: DescribeRiskConfigurationResponse)
{-# DEPRECATED drcrsRiskConfiguration "Use generic-lens or generic-optics with 'riskConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsResponseStatus :: Lens.Lens' DescribeRiskConfigurationResponse Lude.Int
drcrsResponseStatus = Lens.lens (responseStatus :: DescribeRiskConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRiskConfigurationResponse)
{-# DEPRECATED drcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
