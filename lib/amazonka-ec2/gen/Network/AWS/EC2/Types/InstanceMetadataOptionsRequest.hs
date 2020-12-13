{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMetadataOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMetadataOptionsRequest
  ( InstanceMetadataOptionsRequest (..),

    -- * Smart constructor
    mkInstanceMetadataOptionsRequest,

    -- * Lenses
    imorHTTPEndpoint,
    imorHTTPPutResponseHopLimit,
    imorHTTPTokens,
  )
where

import Network.AWS.EC2.Types.HTTPTokensState
import Network.AWS.EC2.Types.InstanceMetadataEndpointState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata options for the instance.
--
-- /See:/ 'mkInstanceMetadataOptionsRequest' smart constructor.
data InstanceMetadataOptionsRequest = InstanceMetadataOptionsRequest'
  { -- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
    hTTPEndpoint :: Lude.Maybe InstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
    --
    -- Default: 1
    -- Possible values: Integers from 1 to 64
    hTTPPutResponseHopLimit :: Lude.Maybe Lude.Int,
    -- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
    --
    -- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
    -- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
    hTTPTokens :: Lude.Maybe HTTPTokensState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceMetadataOptionsRequest' with the minimum fields required to make a request.
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
mkInstanceMetadataOptionsRequest ::
  InstanceMetadataOptionsRequest
mkInstanceMetadataOptionsRequest =
  InstanceMetadataOptionsRequest'
    { hTTPEndpoint = Lude.Nothing,
      hTTPPutResponseHopLimit = Lude.Nothing,
      hTTPTokens = Lude.Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'hTTPEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorHTTPEndpoint :: Lens.Lens' InstanceMetadataOptionsRequest (Lude.Maybe InstanceMetadataEndpointState)
imorHTTPEndpoint = Lens.lens (hTTPEndpoint :: InstanceMetadataOptionsRequest -> Lude.Maybe InstanceMetadataEndpointState) (\s a -> s {hTTPEndpoint = a} :: InstanceMetadataOptionsRequest)
{-# DEPRECATED imorHTTPEndpoint "Use generic-lens or generic-optics with 'hTTPEndpoint' instead." #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'hTTPPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorHTTPPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptionsRequest (Lude.Maybe Lude.Int)
imorHTTPPutResponseHopLimit = Lens.lens (hTTPPutResponseHopLimit :: InstanceMetadataOptionsRequest -> Lude.Maybe Lude.Int) (\s a -> s {hTTPPutResponseHopLimit = a} :: InstanceMetadataOptionsRequest)
{-# DEPRECATED imorHTTPPutResponseHopLimit "Use generic-lens or generic-optics with 'hTTPPutResponseHopLimit' instead." #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'hTTPTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorHTTPTokens :: Lens.Lens' InstanceMetadataOptionsRequest (Lude.Maybe HTTPTokensState)
imorHTTPTokens = Lens.lens (hTTPTokens :: InstanceMetadataOptionsRequest -> Lude.Maybe HTTPTokensState) (\s a -> s {hTTPTokens = a} :: InstanceMetadataOptionsRequest)
{-# DEPRECATED imorHTTPTokens "Use generic-lens or generic-optics with 'hTTPTokens' instead." #-}

instance Lude.ToQuery InstanceMetadataOptionsRequest where
  toQuery InstanceMetadataOptionsRequest' {..} =
    Lude.mconcat
      [ "HttpEndpoint" Lude.=: hTTPEndpoint,
        "HttpPutResponseHopLimit" Lude.=: hTTPPutResponseHopLimit,
        "HttpTokens" Lude.=: hTTPTokens
      ]
