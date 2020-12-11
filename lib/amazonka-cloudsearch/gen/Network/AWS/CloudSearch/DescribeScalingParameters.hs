{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeScalingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the scaling parameters configured for a domain. A domain's scaling parameters specify the desired search instance type and replication count. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-scaling-options.html Configuring Scaling Options> in the /Amazon CloudSearch Developer Guide/ .
module Network.AWS.CloudSearch.DescribeScalingParameters
  ( -- * Creating a request
    DescribeScalingParameters (..),
    mkDescribeScalingParameters,

    -- ** Request lenses
    dspDomainName,

    -- * Destructuring the response
    DescribeScalingParametersResponse (..),
    mkDescribeScalingParametersResponse,

    -- ** Response lenses
    dsprsResponseStatus,
    dsprsScalingParameters,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeScalingParameters' @ operation. Specifies the name of the domain you want to describe.
--
-- /See:/ 'mkDescribeScalingParameters' smart constructor.
newtype DescribeScalingParameters = DescribeScalingParameters'
  { domainName ::
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

-- | Creates a value of 'DescribeScalingParameters' with the minimum fields required to make a request.
--
-- * 'domainName' - Undocumented field.
mkDescribeScalingParameters ::
  -- | 'domainName'
  Lude.Text ->
  DescribeScalingParameters
mkDescribeScalingParameters pDomainName_ =
  DescribeScalingParameters' {domainName = pDomainName_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dspDomainName :: Lens.Lens' DescribeScalingParameters Lude.Text
dspDomainName = Lens.lens (domainName :: DescribeScalingParameters -> Lude.Text) (\s a -> s {domainName = a} :: DescribeScalingParameters)
{-# DEPRECATED dspDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DescribeScalingParameters where
  type
    Rs DescribeScalingParameters =
      DescribeScalingParametersResponse
  request = Req.postQuery cloudSearchService
  response =
    Res.receiveXMLWrapper
      "DescribeScalingParametersResult"
      ( \s h x ->
          DescribeScalingParametersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "ScalingParameters")
      )

instance Lude.ToHeaders DescribeScalingParameters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeScalingParameters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScalingParameters where
  toQuery DescribeScalingParameters' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeScalingParameters" :: Lude.ByteString),
        "Version" Lude.=: ("2013-01-01" :: Lude.ByteString),
        "DomainName" Lude.=: domainName
      ]

-- | The result of a @DescribeScalingParameters@ request. Contains the scaling parameters configured for the domain specified in the request.
--
-- /See:/ 'mkDescribeScalingParametersResponse' smart constructor.
data DescribeScalingParametersResponse = DescribeScalingParametersResponse'
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

-- | Creates a value of 'DescribeScalingParametersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'scalingParameters' - Undocumented field.
mkDescribeScalingParametersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'scalingParameters'
  ScalingParametersStatus ->
  DescribeScalingParametersResponse
mkDescribeScalingParametersResponse
  pResponseStatus_
  pScalingParameters_ =
    DescribeScalingParametersResponse'
      { responseStatus =
          pResponseStatus_,
        scalingParameters = pScalingParameters_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsResponseStatus :: Lens.Lens' DescribeScalingParametersResponse Lude.Int
dsprsResponseStatus = Lens.lens (responseStatus :: DescribeScalingParametersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScalingParametersResponse)
{-# DEPRECATED dsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scalingParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprsScalingParameters :: Lens.Lens' DescribeScalingParametersResponse ScalingParametersStatus
dsprsScalingParameters = Lens.lens (scalingParameters :: DescribeScalingParametersResponse -> ScalingParametersStatus) (\s a -> s {scalingParameters = a} :: DescribeScalingParametersResponse)
{-# DEPRECATED dsprsScalingParameters "Use generic-lens or generic-optics with 'scalingParameters' instead." #-}
