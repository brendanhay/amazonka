{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
  ( LaunchTemplateInstanceMetadataOptions (..),

    -- * Smart constructor
    mkLaunchTemplateInstanceMetadataOptions,

    -- * Lenses
    ltimoState,
    ltimoHTTPEndpoint,
    ltimoHTTPPutResponseHopLimit,
    ltimoHTTPTokens,
  )
where

import Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The metadata options for the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkLaunchTemplateInstanceMetadataOptions' smart constructor.
data LaunchTemplateInstanceMetadataOptions = LaunchTemplateInstanceMetadataOptions'
  { -- | The state of the metadata option changes.
    --
    -- @pending@ - The metadata options are being updated and the instance is not ready to process metadata traffic with the new selection.
    -- @applied@ - The metadata options have been successfully applied on the instance.
    state :: Lude.Maybe LaunchTemplateInstanceMetadataOptionsState,
    -- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
    hTTPEndpoint :: Lude.Maybe LaunchTemplateInstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
    --
    -- Default: 1
    -- Possible values: Integers from 1 to 64
    hTTPPutResponseHopLimit :: Lude.Maybe Lude.Int,
    -- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
    --
    -- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
    -- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
    hTTPTokens :: Lude.Maybe LaunchTemplateHTTPTokensState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateInstanceMetadataOptions' with the minimum fields required to make a request.
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
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
mkLaunchTemplateInstanceMetadataOptions ::
  LaunchTemplateInstanceMetadataOptions
mkLaunchTemplateInstanceMetadataOptions =
  LaunchTemplateInstanceMetadataOptions'
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
ltimoState :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Lude.Maybe LaunchTemplateInstanceMetadataOptionsState)
ltimoState = Lens.lens (state :: LaunchTemplateInstanceMetadataOptions -> Lude.Maybe LaunchTemplateInstanceMetadataOptionsState) (\s a -> s {state = a} :: LaunchTemplateInstanceMetadataOptions)
{-# DEPRECATED ltimoState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | This parameter enables or disables the HTTP metadata endpoint on your instances. If the parameter is not specified, the default state is @enabled@ .
--
-- /Note:/ Consider using 'hTTPEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoHTTPEndpoint :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Lude.Maybe LaunchTemplateInstanceMetadataEndpointState)
ltimoHTTPEndpoint = Lens.lens (hTTPEndpoint :: LaunchTemplateInstanceMetadataOptions -> Lude.Maybe LaunchTemplateInstanceMetadataEndpointState) (\s a -> s {hTTPEndpoint = a} :: LaunchTemplateInstanceMetadataOptions)
{-# DEPRECATED ltimoHTTPEndpoint "Use generic-lens or generic-optics with 'hTTPEndpoint' instead." #-}

-- | The desired HTTP PUT response hop limit for instance metadata requests. The larger the number, the further instance metadata requests can travel.
--
-- Default: 1
-- Possible values: Integers from 1 to 64
--
-- /Note:/ Consider using 'hTTPPutResponseHopLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoHTTPPutResponseHopLimit :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Lude.Maybe Lude.Int)
ltimoHTTPPutResponseHopLimit = Lens.lens (hTTPPutResponseHopLimit :: LaunchTemplateInstanceMetadataOptions -> Lude.Maybe Lude.Int) (\s a -> s {hTTPPutResponseHopLimit = a} :: LaunchTemplateInstanceMetadataOptions)
{-# DEPRECATED ltimoHTTPPutResponseHopLimit "Use generic-lens or generic-optics with 'hTTPPutResponseHopLimit' instead." #-}

-- | The state of token usage for your instance metadata requests. If the parameter is not specified in the request, the default state is @optional@ .
--
-- If the state is @optional@ , you can choose to retrieve instance metadata with or without a signed token header on your request. If you retrieve the IAM role credentials without a token, the version 1.0 role credentials are returned. If you retrieve the IAM role credentials using a valid signed token, the version 2.0 role credentials are returned.
-- If the state is @required@ , you must send a signed token header with any instance metadata retrieval requests. In this state, retrieving the IAM role credentials always returns the version 2.0 credentials; the version 1.0 credentials are not available.
--
-- /Note:/ Consider using 'hTTPTokens' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoHTTPTokens :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Lude.Maybe LaunchTemplateHTTPTokensState)
ltimoHTTPTokens = Lens.lens (hTTPTokens :: LaunchTemplateInstanceMetadataOptions -> Lude.Maybe LaunchTemplateHTTPTokensState) (\s a -> s {hTTPTokens = a} :: LaunchTemplateInstanceMetadataOptions)
{-# DEPRECATED ltimoHTTPTokens "Use generic-lens or generic-optics with 'hTTPTokens' instead." #-}

instance Lude.FromXML LaunchTemplateInstanceMetadataOptions where
  parseXML x =
    LaunchTemplateInstanceMetadataOptions'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "httpEndpoint")
      Lude.<*> (x Lude..@? "httpPutResponseHopLimit")
      Lude.<*> (x Lude..@? "httpTokens")
