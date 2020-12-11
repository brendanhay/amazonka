{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceMetadataOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the instance metadata parameters on a running or stopped instance. When you modify the parameters on a stopped instance, they are applied when the instance is started. When you modify the parameters on a running instance, the API responds with a state of “pending”. After the parameter modifications are successfully applied to the instance, the state of the modifications changes from “pending” to “applied” in subsequent describe-instances API calls. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data> .
module Network.AWS.EC2.ModifyInstanceMetadataOptions
  ( -- * Creating a request
    ModifyInstanceMetadataOptions (..),
    mkModifyInstanceMetadataOptions,

    -- ** Request lenses
    mimoHTTPEndpoint,
    mimoHTTPPutResponseHopLimit,
    mimoHTTPTokens,
    mimoDryRun,
    mimoInstanceId,

    -- * Destructuring the response
    ModifyInstanceMetadataOptionsResponse (..),
    mkModifyInstanceMetadataOptionsResponse,

    -- ** Response lenses
    mimorsInstanceId,
    mimorsInstanceMetadataOptions,
    mimorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyInstanceMetadataOptions' smart constructor.
data ModifyInstanceMetadataOptions = ModifyInstanceMetadataOptions'
  { hTTPEndpoint ::
      Lude.Maybe
        InstanceMetadataEndpointState,
    hTTPPutResponseHopLimit ::
      Lude.Maybe Lude.Int,
    hTTPTokens ::
      Lude.Maybe HTTPTokensState,
    dryRun :: Lude.Maybe Lude.Bool,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceMetadataOptions' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'hTTPEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the existing state is maintained.
-- * 'hTTPPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. If no parameter is specified, the existing state is maintained.
--
-- Possible values: Integers from 1 to 64
-- * 'hTTPTokens' - The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
-- * 'instanceId' - The ID of the instance.
mkModifyInstanceMetadataOptions ::
  -- | 'instanceId'
  Lude.Text ->
  ModifyInstanceMetadataOptions
mkModifyInstanceMetadataOptions pInstanceId_ =
  ModifyInstanceMetadataOptions'
    { hTTPEndpoint = Lude.Nothing,
      hTTPPutResponseHopLimit = Lude.Nothing,
      hTTPTokens = Lude.Nothing,
      dryRun = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the existing state is maintained.
--
-- /Note:/ Consider using 'hTTPEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mimoHTTPEndpoint :: Lens.Lens' ModifyInstanceMetadataOptions (Lude.Maybe InstanceMetadataEndpointState)
mimoHTTPEndpoint = Lens.lens (hTTPEndpoint :: ModifyInstanceMetadataOptions -> Lude.Maybe InstanceMetadataEndpointState) (\s a -> s {hTTPEndpoint = a} :: ModifyInstanceMetadataOptions)
{-# DEPRECATED mimoHTTPEndpoint "Use generic-lens or generic-optics with 'hTTPEndpoint' instead." #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel. If no parameter is specified, the existing state is maintained.
--
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'hTTPPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mimoHTTPPutResponseHopLimit :: Lens.Lens' ModifyInstanceMetadataOptions (Lude.Maybe Lude.Int)
mimoHTTPPutResponseHopLimit = Lens.lens (hTTPPutResponseHopLimit :: ModifyInstanceMetadataOptions -> Lude.Maybe Lude.Int) (\s a -> s {hTTPPutResponseHopLimit = a} :: ModifyInstanceMetadataOptions)
{-# DEPRECATED mimoHTTPPutResponseHopLimit "Use generic-lens or generic-optics with 'hTTPPutResponseHopLimit' instead." #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'hTTPTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mimoHTTPTokens :: Lens.Lens' ModifyInstanceMetadataOptions (Lude.Maybe HTTPTokensState)
mimoHTTPTokens = Lens.lens (hTTPTokens :: ModifyInstanceMetadataOptions -> Lude.Maybe HTTPTokensState) (\s a -> s {hTTPTokens = a} :: ModifyInstanceMetadataOptions)
{-# DEPRECATED mimoHTTPTokens "Use generic-lens or generic-optics with 'hTTPTokens' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mimoDryRun :: Lens.Lens' ModifyInstanceMetadataOptions (Lude.Maybe Lude.Bool)
mimoDryRun = Lens.lens (dryRun :: ModifyInstanceMetadataOptions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyInstanceMetadataOptions)
{-# DEPRECATED mimoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mimoInstanceId :: Lens.Lens' ModifyInstanceMetadataOptions Lude.Text
mimoInstanceId = Lens.lens (instanceId :: ModifyInstanceMetadataOptions -> Lude.Text) (\s a -> s {instanceId = a} :: ModifyInstanceMetadataOptions)
{-# DEPRECATED mimoInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest ModifyInstanceMetadataOptions where
  type
    Rs ModifyInstanceMetadataOptions =
      ModifyInstanceMetadataOptionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyInstanceMetadataOptionsResponse'
            Lude.<$> (x Lude..@? "instanceId")
            Lude.<*> (x Lude..@? "instanceMetadataOptions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyInstanceMetadataOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyInstanceMetadataOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyInstanceMetadataOptions where
  toQuery ModifyInstanceMetadataOptions' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyInstanceMetadataOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "HttpEndpoint" Lude.=: hTTPEndpoint,
        "HttpPutResponseHopLimit" Lude.=: hTTPPutResponseHopLimit,
        "HttpTokens" Lude.=: hTTPTokens,
        "DryRun" Lude.=: dryRun,
        "InstanceId" Lude.=: instanceId
      ]

-- | /See:/ 'mkModifyInstanceMetadataOptionsResponse' smart constructor.
data ModifyInstanceMetadataOptionsResponse = ModifyInstanceMetadataOptionsResponse'
  { instanceId ::
      Lude.Maybe
        Lude.Text,
    instanceMetadataOptions ::
      Lude.Maybe
        InstanceMetadataOptionsResponse,
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

-- | Creates a value of 'ModifyInstanceMetadataOptionsResponse' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'instanceMetadataOptions' - The metadata options for the instance.
-- * 'responseStatus' - The response status code.
mkModifyInstanceMetadataOptionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyInstanceMetadataOptionsResponse
mkModifyInstanceMetadataOptionsResponse pResponseStatus_ =
  ModifyInstanceMetadataOptionsResponse'
    { instanceId = Lude.Nothing,
      instanceMetadataOptions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mimorsInstanceId :: Lens.Lens' ModifyInstanceMetadataOptionsResponse (Lude.Maybe Lude.Text)
mimorsInstanceId = Lens.lens (instanceId :: ModifyInstanceMetadataOptionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ModifyInstanceMetadataOptionsResponse)
{-# DEPRECATED mimorsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The metadata options for the instance.
--
-- /Note:/ Consider using 'instanceMetadataOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mimorsInstanceMetadataOptions :: Lens.Lens' ModifyInstanceMetadataOptionsResponse (Lude.Maybe InstanceMetadataOptionsResponse)
mimorsInstanceMetadataOptions = Lens.lens (instanceMetadataOptions :: ModifyInstanceMetadataOptionsResponse -> Lude.Maybe InstanceMetadataOptionsResponse) (\s a -> s {instanceMetadataOptions = a} :: ModifyInstanceMetadataOptionsResponse)
{-# DEPRECATED mimorsInstanceMetadataOptions "Use generic-lens or generic-optics with 'instanceMetadataOptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mimorsResponseStatus :: Lens.Lens' ModifyInstanceMetadataOptionsResponse Lude.Int
mimorsResponseStatus = Lens.lens (responseStatus :: ModifyInstanceMetadataOptionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyInstanceMetadataOptionsResponse)
{-# DEPRECATED mimorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
