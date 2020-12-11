{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateScalingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures scaling parameters for a domain. A domain's scaling parameters specify the desired search instance type and replication count. Amazon CloudSearch will still automatically scale your domain based on the volume of data and traffic, but not below the desired instance type and replication count. If the Multi-AZ option is enabled, these values control the resources used per Availability Zone. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-scaling-options.html Configuring Scaling Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.UpdateScalingParameters
  ( -- * Creating a request
    UpdateScalingParameters (..),
    mkUpdateScalingParameters,

    -- ** Request lenses
    uspDomainName,
    uspScalingParameters,

    -- * Destructuring the response
    UpdateScalingParametersResponse (..),
    mkUpdateScalingParametersResponse,

    -- ** Response lenses
    usprsResponseStatus,
    usprsScalingParameters,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'UpdateScalingParameters' @ operation. Specifies the name of the domain you want to update and the scaling parameters you want to configure.
--
-- /See:/ 'mkUpdateScalingParameters' smart constructor.
data UpdateScalingParameters = UpdateScalingParameters'
  { domainName ::
      Lude.Text,
    scalingParameters :: ScalingParameters
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateScalingParameters' with the minimum fields required to make a request.
--
-- * 'domainName' - Undocumented field.
-- * 'scalingParameters' - Undocumented field.
mkUpdateScalingParameters ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'scalingParameters'
  ScalingParameters ->
  UpdateScalingParameters
mkUpdateScalingParameters pDomainName_ pScalingParameters_ =
  UpdateScalingParameters'
    { domainName = pDomainName_,
      scalingParameters = pScalingParameters_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspDomainName :: Lens.Lens' UpdateScalingParameters Lude.Text
uspDomainName = Lens.lens (domainName :: UpdateScalingParameters -> Lude.Text) (\s a -> s {domainName = a} :: UpdateScalingParameters)
{-# DEPRECATED uspDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scalingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uspScalingParameters :: Lens.Lens' UpdateScalingParameters ScalingParameters
uspScalingParameters = Lens.lens (scalingParameters :: UpdateScalingParameters -> ScalingParameters) (\s a -> s {scalingParameters = a} :: UpdateScalingParameters)
{-# DEPRECATED uspScalingParameters "Use generic-lens or generic-optics with 'scalingParameters' instead." #-}

instance Lude.AWSRequest UpdateScalingParameters where
  type Rs UpdateScalingParameters = UpdateScalingParametersResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "UpdateScalingParametersResult"
      ( \s h x ->
          UpdateScalingParametersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "ScalingParameters")
      )

instance Lude.ToHeaders UpdateScalingParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateScalingParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateScalingParameters where
  toQuery UpdateScalingParameters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateScalingParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        "ScalingParameters" Lude.=: scalingParameters
      ]

-- | The result of a @UpdateScalingParameters@ request. Contains the status of the newly-configured scaling parameters.
--
-- /See:/ 'mkUpdateScalingParametersResponse' smart constructor.
data UpdateScalingParametersResponse = UpdateScalingParametersResponse'
  { responseStatus ::
      Lude.Int,
    scalingParameters ::
      ScalingParametersStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateScalingParametersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'scalingParameters' - Undocumented field.
mkUpdateScalingParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'scalingParameters'
  ScalingParametersStatus ->
  UpdateScalingParametersResponse
mkUpdateScalingParametersResponse
  pResponseStatus_
  pScalingParameters_ =
    UpdateScalingParametersResponse'
      { responseStatus =
          pResponseStatus_,
        scalingParameters = pScalingParameters_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsResponseStatus :: Lens.Lens' UpdateScalingParametersResponse Lude.Int
usprsResponseStatus = Lens.lens (responseStatus :: UpdateScalingParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateScalingParametersResponse)
{-# DEPRECATED usprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scalingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usprsScalingParameters :: Lens.Lens' UpdateScalingParametersResponse ScalingParametersStatus
usprsScalingParameters = Lens.lens (scalingParameters :: UpdateScalingParametersResponse -> ScalingParametersStatus) (\s a -> s {scalingParameters = a} :: UpdateScalingParametersResponse)
{-# DEPRECATED usprsScalingParameters "Use generic-lens or generic-optics with 'scalingParameters' instead." #-}
