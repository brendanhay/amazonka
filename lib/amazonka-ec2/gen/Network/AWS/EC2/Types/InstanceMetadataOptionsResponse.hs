{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMetadataOptionsResponse
  ( InstanceMetadataOptionsResponse (..),

    -- * Smart constructor
    mkInstanceMetadataOptionsResponse,

    -- * Lenses
    imoState,
    imoHTTPEndpoint,
    imoHTTPPutResponseHopLimit,
    imoHTTPTokens,
  )
where

import Network.AWS.EC2.Types.HTTPTokensState
import Network.AWS.EC2.Types.InstanceMetadataEndpointState
import Network.AWS.EC2.Types.InstanceMetadataOptionsState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata options for the instance.
--
-- /See:/ 'mkInstanceMetadataOptionsResponse' smart constructor.
data InstanceMetadataOptionsResponse = InstanceMetadataOptionsResponse'
  { -- | The state of the metadata option changes.
    --
    -- @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection.
    -- @applied@ - The metadata options have been successfully applied on the instance.
    state :: Lude.Maybe InstanceMetadataOptionsState,
    -- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
    hTTPEndpoint :: Lude.Maybe InstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
    --
    -- Default: 1
    -- Possible values: Integers from 1 to 64
    hTTPPutResponseHopLimit :: Lude.Maybe Lude.Int,
    -- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
    --
    -- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
    -- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
    hTTPTokens :: Lude.Maybe HTTPTokensState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceMetadataOptionsResponse' with the minimum fields required to make a request.
--
-- * 'state' - The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection.
-- @applied@ - The metadata options have been successfully applied on the instance.
-- * 'hTTPEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
-- * 'hTTPPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
-- * 'hTTPTokens' - The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
mkInstanceMetadataOptionsResponse ::
  InstanceMetadataOptionsResponse
mkInstanceMetadataOptionsResponse =
  InstanceMetadataOptionsResponse'
    { state = Lude.Nothing,
      hTTPEndpoint = Lude.Nothing,
      hTTPPutResponseHopLimit = Lude.Nothing,
      hTTPTokens = Lude.Nothing
    }

-- | The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection.
-- @applied@ - The metadata options have been successfully applied on the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoState :: Lens.Lens' InstanceMetadataOptionsResponse (Lude.Maybe InstanceMetadataOptionsState)
imoState = Lens.lens (state :: InstanceMetadataOptionsResponse -> Lude.Maybe InstanceMetadataOptionsState) (\s a -> s {state = a} :: InstanceMetadataOptionsResponse)
{-# DEPRECATED imoState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'hTTPEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHTTPEndpoint :: Lens.Lens' InstanceMetadataOptionsResponse (Lude.Maybe InstanceMetadataEndpointState)
imoHTTPEndpoint = Lens.lens (hTTPEndpoint :: InstanceMetadataOptionsResponse -> Lude.Maybe InstanceMetadataEndpointState) (\s a -> s {hTTPEndpoint = a} :: InstanceMetadataOptionsResponse)
{-# DEPRECATED imoHTTPEndpoint "Use generic-lens or generic-optics with 'hTTPEndpoint' instead." #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'hTTPPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHTTPPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptionsResponse (Lude.Maybe Lude.Int)
imoHTTPPutResponseHopLimit = Lens.lens (hTTPPutResponseHopLimit :: InstanceMetadataOptionsResponse -> Lude.Maybe Lude.Int) (\s a -> s {hTTPPutResponseHopLimit = a} :: InstanceMetadataOptionsResponse)
{-# DEPRECATED imoHTTPPutResponseHopLimit "Use generic-lens or generic-optics with 'hTTPPutResponseHopLimit' instead." #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credential always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'hTTPTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imoHTTPTokens :: Lens.Lens' InstanceMetadataOptionsResponse (Lude.Maybe HTTPTokensState)
imoHTTPTokens = Lens.lens (hTTPTokens :: InstanceMetadataOptionsResponse -> Lude.Maybe HTTPTokensState) (\s a -> s {hTTPTokens = a} :: InstanceMetadataOptionsResponse)
{-# DEPRECATED imoHTTPTokens "Use generic-lens or generic-optics with 'hTTPTokens' instead." #-}

instance Lude.FromXML InstanceMetadataOptionsResponse where
  parseXML x =
    InstanceMetadataOptionsResponse'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "httpEndpoint")
      Lude.<*> (x Lude..@? "httpPutResponseHopLimit")
      Lude.<*> (x Lude..@? "httpTokens")
