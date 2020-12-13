{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceMetadataOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMetadataOptions
  ( InstanceMetadataOptions (..),

    -- * Smart constructor
    mkInstanceMetadataOptions,

    -- * Lenses
    imoHTTPEndpoint,
    imoHTTPPutResponseHopLimit,
    imoHTTPTokens,
  )
where

import Network.AWS.AutoScaling.Types.InstanceMetadataEndpointState
import Network.AWS.AutoScaling.Types.InstanceMetadataHTTPTokensState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata options for the instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkInstanceMetadataOptions' smart constructor.
data InstanceMetadataOptions = InstanceMetadataOptions'
  { -- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
    hTTPEndpoint :: Lude.Maybe InstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
    --
    -- Default: 1
    -- Possible values: Integers from 1 to 64
    hTTPPutResponseHopLimit :: Lude.Maybe Lude.Natural,
    -- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
    --
    -- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
    -- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
    hTTPTokens :: Lude.Maybe InstanceMetadataHTTPTokensState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceMetadataOptions' with the minimum fields required to make a request.
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
mkInstanceMetadataOptions ::
  InstanceMetadataOptions
mkInstanceMetadataOptions =
  InstanceMetadataOptions'
    { hTTPEndpoint = Lude.Nothing,
      hTTPPutResponseHopLimit = Lude.Nothing,
      hTTPTokens = Lude.Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'hTTPEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHTTPEndpoint :: Lens.Lens' InstanceMetadataOptions (Lude.Maybe InstanceMetadataEndpointState)
imoHTTPEndpoint = Lens.lens (hTTPEndpoint :: InstanceMetadataOptions -> Lude.Maybe InstanceMetadataEndpointState) (\s a -> s {hTTPEndpoint = a} :: InstanceMetadataOptions)
{-# DEPRECATED imoHTTPEndpoint "Use generic-lens or generic-optics with 'hTTPEndpoint' instead." #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'hTTPPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHTTPPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptions (Lude.Maybe Lude.Natural)
imoHTTPPutResponseHopLimit = Lens.lens (hTTPPutResponseHopLimit :: InstanceMetadataOptions -> Lude.Maybe Lude.Natural) (\s a -> s {hTTPPutResponseHopLimit = a} :: InstanceMetadataOptions)
{-# DEPRECATED imoHTTPPutResponseHopLimit "Use generic-lens or generic-optics with 'hTTPPutResponseHopLimit' instead." #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'hTTPTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHTTPTokens :: Lens.Lens' InstanceMetadataOptions (Lude.Maybe InstanceMetadataHTTPTokensState)
imoHTTPTokens = Lens.lens (hTTPTokens :: InstanceMetadataOptions -> Lude.Maybe InstanceMetadataHTTPTokensState) (\s a -> s {hTTPTokens = a} :: InstanceMetadataOptions)
{-# DEPRECATED imoHTTPTokens "Use generic-lens or generic-optics with 'hTTPTokens' instead." #-}

instance Lude.FromXML InstanceMetadataOptions where
  parseXML x =
    InstanceMetadataOptions'
      Lude.<$> (x Lude..@? "HttpEndpoint")
      Lude.<*> (x Lude..@? "HttpPutResponseHopLimit")
      Lude.<*> (x Lude..@? "HttpTokens")

instance Lude.ToQuery InstanceMetadataOptions where
  toQuery InstanceMetadataOptions' {..} =
    Lude.mconcat
      [ "HttpEndpoint" Lude.=: hTTPEndpoint,
        "HttpPutResponseHopLimit" Lude.=: hTTPPutResponseHopLimit,
        "HttpTokens" Lude.=: hTTPTokens
      ]
