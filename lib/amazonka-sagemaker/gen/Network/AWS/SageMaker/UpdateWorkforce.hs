{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateWorkforce
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to update your workforce. You can use this operation to require that workers use specific IP addresses to work on tasks and to update your OpenID Connect (OIDC) Identity Provider (IdP) workforce configuration.
--
-- Use @SourceIpConfig@ to restrict worker access to tasks to a specific range of IP addresses. You specify allowed IP addresses by creating a list of up to ten <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> . By default, a workforce isn't restricted to specific IP addresses. If you specify a range of IP addresses, workers who attempt to access tasks using any IP address outside the specified range are denied and get a @Not Found@ error message on the worker portal.
-- Use @OidcConfig@ to update the configuration of a workforce created using your own OIDC IdP.
-- /Important:/ You can only update your OIDC IdP configuration when there are no work teams associated with your workforce. You can delete work teams using the operation.
-- After restricting access to a range of IP addresses or updating your OIDC IdP configuration with this operation, you can view details about your update workforce using the operation.
-- /Important:/ This operation only applies to private workforces.
module Network.AWS.SageMaker.UpdateWorkforce
  ( -- * Creating a request
    UpdateWorkforce (..),
    mkUpdateWorkforce,

    -- ** Request lenses
    uwSourceIPConfig,
    uwOidcConfig,
    uwWorkforceName,

    -- * Destructuring the response
    UpdateWorkforceResponse (..),
    mkUpdateWorkforceResponse,

    -- ** Response lenses
    updrsResponseStatus,
    updrsWorkforce,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateWorkforce' smart constructor.
data UpdateWorkforce = UpdateWorkforce'
  { sourceIPConfig ::
      Lude.Maybe SourceIPConfig,
    oidcConfig :: Lude.Maybe OidcConfig,
    workforceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkforce' with the minimum fields required to make a request.
--
-- * 'oidcConfig' - Use this parameter to update your OIDC Identity Provider (IdP) configuration for a workforce made using your own IdP.
-- * 'sourceIPConfig' - A list of one to ten worker IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) that can be used to access tasks assigned to this workforce.
--
-- Maximum: Ten CIDR values
-- * 'workforceName' - The name of the private workforce that you want to update. You can find your workforce name by using the operation.
mkUpdateWorkforce ::
  -- | 'workforceName'
  Lude.Text ->
  UpdateWorkforce
mkUpdateWorkforce pWorkforceName_ =
  UpdateWorkforce'
    { sourceIPConfig = Lude.Nothing,
      oidcConfig = Lude.Nothing,
      workforceName = pWorkforceName_
    }

-- | A list of one to ten worker IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ) that can be used to access tasks assigned to this workforce.
--
-- Maximum: Ten CIDR values
--
-- /Note:/ Consider using 'sourceIPConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwSourceIPConfig :: Lens.Lens' UpdateWorkforce (Lude.Maybe SourceIPConfig)
uwSourceIPConfig = Lens.lens (sourceIPConfig :: UpdateWorkforce -> Lude.Maybe SourceIPConfig) (\s a -> s {sourceIPConfig = a} :: UpdateWorkforce)
{-# DEPRECATED uwSourceIPConfig "Use generic-lens or generic-optics with 'sourceIPConfig' instead." #-}

-- | Use this parameter to update your OIDC Identity Provider (IdP) configuration for a workforce made using your own IdP.
--
-- /Note:/ Consider using 'oidcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwOidcConfig :: Lens.Lens' UpdateWorkforce (Lude.Maybe OidcConfig)
uwOidcConfig = Lens.lens (oidcConfig :: UpdateWorkforce -> Lude.Maybe OidcConfig) (\s a -> s {oidcConfig = a} :: UpdateWorkforce)
{-# DEPRECATED uwOidcConfig "Use generic-lens or generic-optics with 'oidcConfig' instead." #-}

-- | The name of the private workforce that you want to update. You can find your workforce name by using the operation.
--
-- /Note:/ Consider using 'workforceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwWorkforceName :: Lens.Lens' UpdateWorkforce Lude.Text
uwWorkforceName = Lens.lens (workforceName :: UpdateWorkforce -> Lude.Text) (\s a -> s {workforceName = a} :: UpdateWorkforce)
{-# DEPRECATED uwWorkforceName "Use generic-lens or generic-optics with 'workforceName' instead." #-}

instance Lude.AWSRequest UpdateWorkforce where
  type Rs UpdateWorkforce = UpdateWorkforceResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateWorkforceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "Workforce")
      )

instance Lude.ToHeaders UpdateWorkforce where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateWorkforce" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateWorkforce where
  toJSON UpdateWorkforce' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceIpConfig" Lude..=) Lude.<$> sourceIPConfig,
            ("OidcConfig" Lude..=) Lude.<$> oidcConfig,
            Lude.Just ("WorkforceName" Lude..= workforceName)
          ]
      )

instance Lude.ToPath UpdateWorkforce where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateWorkforce where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateWorkforceResponse' smart constructor.
data UpdateWorkforceResponse = UpdateWorkforceResponse'
  { responseStatus ::
      Lude.Int,
    workforce :: Workforce
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateWorkforceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'workforce' - A single private workforce. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
mkUpdateWorkforceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'workforce'
  Workforce ->
  UpdateWorkforceResponse
mkUpdateWorkforceResponse pResponseStatus_ pWorkforce_ =
  UpdateWorkforceResponse'
    { responseStatus = pResponseStatus_,
      workforce = pWorkforce_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsResponseStatus :: Lens.Lens' UpdateWorkforceResponse Lude.Int
updrsResponseStatus = Lens.lens (responseStatus :: UpdateWorkforceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateWorkforceResponse)
{-# DEPRECATED updrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A single private workforce. You can create one private work force in each AWS Region. By default, any workforce-related API operation used in a specific region will apply to the workforce created in that region. To learn how to create a private workforce, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-workforce-create-private.html Create a Private Workforce> .
--
-- /Note:/ Consider using 'workforce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsWorkforce :: Lens.Lens' UpdateWorkforceResponse Workforce
updrsWorkforce = Lens.lens (workforce :: UpdateWorkforceResponse -> Workforce) (\s a -> s {workforce = a} :: UpdateWorkforceResponse)
{-# DEPRECATED updrsWorkforce "Use generic-lens or generic-optics with 'workforce' instead." #-}
