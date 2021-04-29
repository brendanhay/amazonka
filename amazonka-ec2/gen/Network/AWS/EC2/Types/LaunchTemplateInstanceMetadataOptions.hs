{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LaunchTemplateHttpTokensState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataEndpointState
import Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataOptionsState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata options for the instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance Metadata and User Data>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ 'newLaunchTemplateInstanceMetadataOptions' smart constructor.
data LaunchTemplateInstanceMetadataOptions = LaunchTemplateInstanceMetadataOptions'
  { -- | This parameter enables or disables the HTTP metadata endpoint on your
    -- instances. If the parameter is not specified, the default state is
    -- @enabled@.
    --
    -- If you specify a value of @disabled@, you will not be able to access
    -- your instance metadata.
    httpEndpoint :: Prelude.Maybe LaunchTemplateInstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    --
    -- Default: 1
    --
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
    -- | The state of the metadata option changes.
    --
    -- @pending@ - The metadata options are being updated and the instance is
    -- not ready to process metadata traffic with the new selection.
    --
    -- @applied@ - The metadata options have been successfully applied on the
    -- instance.
    state :: Prelude.Maybe LaunchTemplateInstanceMetadataOptionsState,
    -- | The state of token usage for your instance metadata requests. If the
    -- parameter is not specified in the request, the default state is
    -- @optional@.
    --
    -- If the state is @optional@, you can choose to retrieve instance metadata
    -- with or without a signed token header on your request. If you retrieve
    -- the IAM role credentials without a token, the version 1.0 role
    -- credentials are returned. If you retrieve the IAM role credentials using
    -- a valid signed token, the version 2.0 role credentials are returned.
    --
    -- If the state is @required@, you must send a signed token header with any
    -- instance metadata retrieval requests. In this state, retrieving the IAM
    -- role credentials always returns the version 2.0 credentials; the version
    -- 1.0 credentials are not available.
    httpTokens :: Prelude.Maybe LaunchTemplateHttpTokensState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceMetadataOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'launchTemplateInstanceMetadataOptions_httpEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the default state is
-- @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
--
-- 'httpPutResponseHopLimit', 'launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
--
-- 'state', 'launchTemplateInstanceMetadataOptions_state' - The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is
-- not ready to process metadata traffic with the new selection.
--
-- @applied@ - The metadata options have been successfully applied on the
-- instance.
--
-- 'httpTokens', 'launchTemplateInstanceMetadataOptions_httpTokens' - The state of token usage for your instance metadata requests. If the
-- parameter is not specified in the request, the default state is
-- @optional@.
--
-- If the state is @optional@, you can choose to retrieve instance metadata
-- with or without a signed token header on your request. If you retrieve
-- the IAM role credentials without a token, the version 1.0 role
-- credentials are returned. If you retrieve the IAM role credentials using
-- a valid signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with any
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credentials always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
newLaunchTemplateInstanceMetadataOptions ::
  LaunchTemplateInstanceMetadataOptions
newLaunchTemplateInstanceMetadataOptions =
  LaunchTemplateInstanceMetadataOptions'
    { httpEndpoint =
        Prelude.Nothing,
      httpPutResponseHopLimit =
        Prelude.Nothing,
      state = Prelude.Nothing,
      httpTokens = Prelude.Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the default state is
-- @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
launchTemplateInstanceMetadataOptions_httpEndpoint :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Prelude.Maybe LaunchTemplateInstanceMetadataEndpointState)
launchTemplateInstanceMetadataOptions_httpEndpoint = Lens.lens (\LaunchTemplateInstanceMetadataOptions' {httpEndpoint} -> httpEndpoint) (\s@LaunchTemplateInstanceMetadataOptions' {} a -> s {httpEndpoint = a} :: LaunchTemplateInstanceMetadataOptions)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Prelude.Maybe Prelude.Int)
launchTemplateInstanceMetadataOptions_httpPutResponseHopLimit = Lens.lens (\LaunchTemplateInstanceMetadataOptions' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@LaunchTemplateInstanceMetadataOptions' {} a -> s {httpPutResponseHopLimit = a} :: LaunchTemplateInstanceMetadataOptions)

-- | The state of the metadata option changes.
--
-- @pending@ - The metadata options are being updated and the instance is
-- not ready to process metadata traffic with the new selection.
--
-- @applied@ - The metadata options have been successfully applied on the
-- instance.
launchTemplateInstanceMetadataOptions_state :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Prelude.Maybe LaunchTemplateInstanceMetadataOptionsState)
launchTemplateInstanceMetadataOptions_state = Lens.lens (\LaunchTemplateInstanceMetadataOptions' {state} -> state) (\s@LaunchTemplateInstanceMetadataOptions' {} a -> s {state = a} :: LaunchTemplateInstanceMetadataOptions)

-- | The state of token usage for your instance metadata requests. If the
-- parameter is not specified in the request, the default state is
-- @optional@.
--
-- If the state is @optional@, you can choose to retrieve instance metadata
-- with or without a signed token header on your request. If you retrieve
-- the IAM role credentials without a token, the version 1.0 role
-- credentials are returned. If you retrieve the IAM role credentials using
-- a valid signed token, the version 2.0 role credentials are returned.
--
-- If the state is @required@, you must send a signed token header with any
-- instance metadata retrieval requests. In this state, retrieving the IAM
-- role credentials always returns the version 2.0 credentials; the version
-- 1.0 credentials are not available.
launchTemplateInstanceMetadataOptions_httpTokens :: Lens.Lens' LaunchTemplateInstanceMetadataOptions (Prelude.Maybe LaunchTemplateHttpTokensState)
launchTemplateInstanceMetadataOptions_httpTokens = Lens.lens (\LaunchTemplateInstanceMetadataOptions' {httpTokens} -> httpTokens) (\s@LaunchTemplateInstanceMetadataOptions' {} a -> s {httpTokens = a} :: LaunchTemplateInstanceMetadataOptions)

instance
  Prelude.FromXML
    LaunchTemplateInstanceMetadataOptions
  where
  parseXML x =
    LaunchTemplateInstanceMetadataOptions'
      Prelude.<$> (x Prelude..@? "httpEndpoint")
      Prelude.<*> (x Prelude..@? "httpPutResponseHopLimit")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "httpTokens")

instance
  Prelude.Hashable
    LaunchTemplateInstanceMetadataOptions

instance
  Prelude.NFData
    LaunchTemplateInstanceMetadataOptions
