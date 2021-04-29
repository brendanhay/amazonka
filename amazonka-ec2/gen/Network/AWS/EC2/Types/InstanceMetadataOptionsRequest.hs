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
-- Module      : Network.AWS.EC2.Types.InstanceMetadataOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMetadataOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.HttpTokensState
import Network.AWS.EC2.Types.InstanceMetadataEndpointState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The metadata options for the instance.
--
-- /See:/ 'newInstanceMetadataOptionsRequest' smart constructor.
data InstanceMetadataOptionsRequest = InstanceMetadataOptionsRequest'
  { -- | This parameter enables or disables the HTTP metadata endpoint on your
    -- instances. If the parameter is not specified, the default state is
    -- @enabled@.
    --
    -- If you specify a value of @disabled@, you will not be able to access
    -- your instance metadata.
    httpEndpoint :: Prelude.Maybe InstanceMetadataEndpointState,
    -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    --
    -- Default: 1
    --
    -- Possible values: Integers from 1 to 64
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Int,
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
    httpTokens :: Prelude.Maybe HttpTokensState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstanceMetadataOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpEndpoint', 'instanceMetadataOptionsRequest_httpEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the default state is
-- @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
--
-- 'httpPutResponseHopLimit', 'instanceMetadataOptionsRequest_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
--
-- 'httpTokens', 'instanceMetadataOptionsRequest_httpTokens' - The state of token usage for your instance metadata requests. If the
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
newInstanceMetadataOptionsRequest ::
  InstanceMetadataOptionsRequest
newInstanceMetadataOptionsRequest =
  InstanceMetadataOptionsRequest'
    { httpEndpoint =
        Prelude.Nothing,
      httpPutResponseHopLimit = Prelude.Nothing,
      httpTokens = Prelude.Nothing
    }

-- | This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the default state is
-- @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
instanceMetadataOptionsRequest_httpEndpoint :: Lens.Lens' InstanceMetadataOptionsRequest (Prelude.Maybe InstanceMetadataEndpointState)
instanceMetadataOptionsRequest_httpEndpoint = Lens.lens (\InstanceMetadataOptionsRequest' {httpEndpoint} -> httpEndpoint) (\s@InstanceMetadataOptionsRequest' {} a -> s {httpEndpoint = a} :: InstanceMetadataOptionsRequest)

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- Possible values: Integers from 1 to 64
instanceMetadataOptionsRequest_httpPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptionsRequest (Prelude.Maybe Prelude.Int)
instanceMetadataOptionsRequest_httpPutResponseHopLimit = Lens.lens (\InstanceMetadataOptionsRequest' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@InstanceMetadataOptionsRequest' {} a -> s {httpPutResponseHopLimit = a} :: InstanceMetadataOptionsRequest)

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
instanceMetadataOptionsRequest_httpTokens :: Lens.Lens' InstanceMetadataOptionsRequest (Prelude.Maybe HttpTokensState)
instanceMetadataOptionsRequest_httpTokens = Lens.lens (\InstanceMetadataOptionsRequest' {httpTokens} -> httpTokens) (\s@InstanceMetadataOptionsRequest' {} a -> s {httpTokens = a} :: InstanceMetadataOptionsRequest)

instance
  Prelude.Hashable
    InstanceMetadataOptionsRequest

instance
  Prelude.NFData
    InstanceMetadataOptionsRequest

instance
  Prelude.ToQuery
    InstanceMetadataOptionsRequest
  where
  toQuery InstanceMetadataOptionsRequest' {..} =
    Prelude.mconcat
      [ "HttpEndpoint" Prelude.=: httpEndpoint,
        "HttpPutResponseHopLimit"
          Prelude.=: httpPutResponseHopLimit,
        "HttpTokens" Prelude.=: httpTokens
      ]
