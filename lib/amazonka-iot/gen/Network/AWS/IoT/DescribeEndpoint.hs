{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique endpoint specific to the AWS account making the call.
module Network.AWS.IoT.DescribeEndpoint
  ( -- * Creating a request
    DescribeEndpoint (..),
    mkDescribeEndpoint,

    -- ** Request lenses
    deEndpointType,

    -- * Destructuring the response
    DescribeEndpointResponse (..),
    mkDescribeEndpointResponse,

    -- ** Response lenses
    dersEndpointAddress,
    dersResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DescribeEndpoint operation.
--
-- /See:/ 'mkDescribeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { -- | The endpoint type. Valid endpoint types include:
    --
    --
    --     * @iot:Data@ - Returns a VeriSign signed data endpoint.
    --
    --
    --
    --     * @iot:Data-ATS@ - Returns an ATS signed data endpoint.
    --
    --
    --
    --     * @iot:CredentialProvider@ - Returns an AWS IoT credentials provider API endpoint.
    --
    --
    --
    --     * @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
    --
    --
    -- We strongly recommend that customers use the newer @iot:Data-ATS@ endpoint type to avoid issues related to the widespread distrust of Symantec certificate authorities.
    endpointType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointType' - The endpoint type. Valid endpoint types include:
--
--
--     * @iot:Data@ - Returns a VeriSign signed data endpoint.
--
--
--
--     * @iot:Data-ATS@ - Returns an ATS signed data endpoint.
--
--
--
--     * @iot:CredentialProvider@ - Returns an AWS IoT credentials provider API endpoint.
--
--
--
--     * @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
--
--
-- We strongly recommend that customers use the newer @iot:Data-ATS@ endpoint type to avoid issues related to the widespread distrust of Symantec certificate authorities.
mkDescribeEndpoint ::
  DescribeEndpoint
mkDescribeEndpoint = DescribeEndpoint' {endpointType = Lude.Nothing}

-- | The endpoint type. Valid endpoint types include:
--
--
--     * @iot:Data@ - Returns a VeriSign signed data endpoint.
--
--
--
--     * @iot:Data-ATS@ - Returns an ATS signed data endpoint.
--
--
--
--     * @iot:CredentialProvider@ - Returns an AWS IoT credentials provider API endpoint.
--
--
--
--     * @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
--
--
-- We strongly recommend that customers use the newer @iot:Data-ATS@ endpoint type to avoid issues related to the widespread distrust of Symantec certificate authorities.
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointType :: Lens.Lens' DescribeEndpoint (Lude.Maybe Lude.Text)
deEndpointType = Lens.lens (endpointType :: DescribeEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointType = a} :: DescribeEndpoint)
{-# DEPRECATED deEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

instance Lude.AWSRequest DescribeEndpoint where
  type Rs DescribeEndpoint = DescribeEndpointResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Lude.<$> (x Lude..?> "endpointAddress")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeEndpoint where
  toPath = Lude.const "/endpoint"

instance Lude.ToQuery DescribeEndpoint where
  toQuery DescribeEndpoint' {..} =
    Lude.mconcat ["endpointType" Lude.=: endpointType]

-- | The output from the DescribeEndpoint operation.
--
-- /See:/ 'mkDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | The endpoint. The format of the endpoint is as follows: /identifier/ .iot./region/ .amazonaws.com.
    endpointAddress :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEndpointResponse' with the minimum fields required to make a request.
--
-- * 'endpointAddress' - The endpoint. The format of the endpoint is as follows: /identifier/ .iot./region/ .amazonaws.com.
-- * 'responseStatus' - The response status code.
mkDescribeEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEndpointResponse
mkDescribeEndpointResponse pResponseStatus_ =
  DescribeEndpointResponse'
    { endpointAddress = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The endpoint. The format of the endpoint is as follows: /identifier/ .iot./region/ .amazonaws.com.
--
-- /Note:/ Consider using 'endpointAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersEndpointAddress :: Lens.Lens' DescribeEndpointResponse (Lude.Maybe Lude.Text)
dersEndpointAddress = Lens.lens (endpointAddress :: DescribeEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {endpointAddress = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersEndpointAddress "Use generic-lens or generic-optics with 'endpointAddress' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dersResponseStatus :: Lens.Lens' DescribeEndpointResponse Lude.Int
dersResponseStatus = Lens.lens (responseStatus :: DescribeEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEndpointResponse)
{-# DEPRECATED dersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
