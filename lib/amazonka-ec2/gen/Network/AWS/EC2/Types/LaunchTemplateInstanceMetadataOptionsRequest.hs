{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsRequest
  ( LaunchTemplateInstanceMetadataOptionsRequest (..),

    -- * Smart constructor
    mkLaunchTemplateInstanceMetadataOptionsRequest,

    -- * Lenses
    ltimorHTTPEndpoint,
    ltimorHTTPPutResponseHopLimit,
    ltimorHTTPTokens,
  )
where

import Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkLaunchTemplateInstanceMetadataOptionsRequest' smart constructor.
data LaunchTemplateInstanceMetadataOptionsRequest = LaunchTemplateInstanceMetadataOptionsRequest'
  { hTTPEndpoint ::
      Lude.Maybe
        LaunchTemplateInstanceMetadataEndpointState,
    hTTPPutResponseHopLimit ::
      Lude.Maybe
        Lude.Int,
    hTTPTokens ::
      Lude.Maybe
        LaunchTemplateHTTPTokensState
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateInstanceMetadataOptionsRequest' with the minimum fields required to make a request.
--
-- * 'hTTPEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
-- * 'hTTPPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
-- * 'hTTPTokens' - The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
mkLaunchTemplateInstanceMetadataOptionsRequest ::
  LaunchTemplateInstanceMetadataOptionsRequest
mkLaunchTemplateInstanceMetadataOptionsRequest =
  LaunchTemplateInstanceMetadataOptionsRequest'
    { hTTPEndpoint =
        Lude.Nothing,
      hTTPPutResponseHopLimit = Lude.Nothing,
      hTTPTokens = Lude.Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'hTTPEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorHTTPEndpoint :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Lude.Maybe LaunchTemplateInstanceMetadataEndpointState)
ltimorHTTPEndpoint = Lens.lens (hTTPEndpoint :: LaunchTemplateInstanceMetadataOptionsRequest -> Lude.Maybe LaunchTemplateInstanceMetadataEndpointState) (\s a -> s {hTTPEndpoint = a} :: LaunchTemplateInstanceMetadataOptionsRequest)
{-# DEPRECATED ltimorHTTPEndpoint "Use generic-lens or generic-optics with 'hTTPEndpoint' instead." #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'hTTPPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorHTTPPutResponseHopLimit :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Lude.Maybe Lude.Int)
ltimorHTTPPutResponseHopLimit = Lens.lens (hTTPPutResponseHopLimit :: LaunchTemplateInstanceMetadataOptionsRequest -> Lude.Maybe Lude.Int) (\s a -> s {hTTPPutResponseHopLimit = a} :: LaunchTemplateInstanceMetadataOptionsRequest)
{-# DEPRECATED ltimorHTTPPutResponseHopLimit "Use generic-lens or generic-optics with 'hTTPPutResponseHopLimit' instead." #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'hTTPTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorHTTPTokens :: Lens.Lens' LaunchTemplateInstanceMetadataOptionsRequest (Lude.Maybe LaunchTemplateHTTPTokensState)
ltimorHTTPTokens = Lens.lens (hTTPTokens :: LaunchTemplateInstanceMetadataOptionsRequest -> Lude.Maybe LaunchTemplateHTTPTokensState) (\s a -> s {hTTPTokens = a} :: LaunchTemplateInstanceMetadataOptionsRequest)
{-# DEPRECATED ltimorHTTPTokens "Use generic-lens or generic-optics with 'hTTPTokens' instead." #-}

instance Lude.ToQuery LaunchTemplateInstanceMetadataOptionsRequest where
  toQuery LaunchTemplateInstanceMetadataOptionsRequest' {..} =
    Lude.mconcat
      [ "HttpEndpoint" Lude.=: hTTPEndpoint,
        "HttpPutResponseHopLimit" Lude.=: hTTPPutResponseHopLimit,
        "HttpTokens" Lude.=: hTTPTokens
      ]
