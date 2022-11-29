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
-- Module      : Amazonka.AutoScaling.Types.InstanceMetadataOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceMetadataOptions where

import Amazonka.AutoScaling.Types.InstanceMetadataEndpointState
import Amazonka.AutoScaling.Types.InstanceMetadataHttpTokensState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata options for the instances. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newInstanceMetadataOptions' smart constructor.
data InstanceMetadataOptions = InstanceMetadataOptions'
  { -- | The desired HTTP PUT response hop limit for instance metadata requests.
    -- The larger the number, the further instance metadata requests can
    -- travel.
    --
    -- Default: 1
    httpPutResponseHopLimit :: Prelude.Maybe Prelude.Natural,
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
    httpTokens :: Prelude.Maybe InstanceMetadataHttpTokensState,
    -- | This parameter enables or disables the HTTP metadata endpoint on your
    -- instances. If the parameter is not specified, the default state is
    -- @enabled@.
    --
    -- If you specify a value of @disabled@, you will not be able to access
    -- your instance metadata.
    httpEndpoint :: Prelude.Maybe InstanceMetadataEndpointState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMetadataOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpPutResponseHopLimit', 'instanceMetadataOptions_httpPutResponseHopLimit' - The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
--
-- 'httpTokens', 'instanceMetadataOptions_httpTokens' - The state of token usage for your instance metadata requests. If the
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
--
-- 'httpEndpoint', 'instanceMetadataOptions_httpEndpoint' - This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the default state is
-- @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
newInstanceMetadataOptions ::
  InstanceMetadataOptions
newInstanceMetadataOptions =
  InstanceMetadataOptions'
    { httpPutResponseHopLimit =
        Prelude.Nothing,
      httpTokens = Prelude.Nothing,
      httpEndpoint = Prelude.Nothing
    }

-- | The desired HTTP PUT response hop limit for instance metadata requests.
-- The larger the number, the further instance metadata requests can
-- travel.
--
-- Default: 1
instanceMetadataOptions_httpPutResponseHopLimit :: Lens.Lens' InstanceMetadataOptions (Prelude.Maybe Prelude.Natural)
instanceMetadataOptions_httpPutResponseHopLimit = Lens.lens (\InstanceMetadataOptions' {httpPutResponseHopLimit} -> httpPutResponseHopLimit) (\s@InstanceMetadataOptions' {} a -> s {httpPutResponseHopLimit = a} :: InstanceMetadataOptions)

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
instanceMetadataOptions_httpTokens :: Lens.Lens' InstanceMetadataOptions (Prelude.Maybe InstanceMetadataHttpTokensState)
instanceMetadataOptions_httpTokens = Lens.lens (\InstanceMetadataOptions' {httpTokens} -> httpTokens) (\s@InstanceMetadataOptions' {} a -> s {httpTokens = a} :: InstanceMetadataOptions)

-- | This parameter enables or disables the HTTP metadata endpoint on your
-- instances. If the parameter is not specified, the default state is
-- @enabled@.
--
-- If you specify a value of @disabled@, you will not be able to access
-- your instance metadata.
instanceMetadataOptions_httpEndpoint :: Lens.Lens' InstanceMetadataOptions (Prelude.Maybe InstanceMetadataEndpointState)
instanceMetadataOptions_httpEndpoint = Lens.lens (\InstanceMetadataOptions' {httpEndpoint} -> httpEndpoint) (\s@InstanceMetadataOptions' {} a -> s {httpEndpoint = a} :: InstanceMetadataOptions)

instance Core.FromXML InstanceMetadataOptions where
  parseXML x =
    InstanceMetadataOptions'
      Prelude.<$> (x Core..@? "HttpPutResponseHopLimit")
      Prelude.<*> (x Core..@? "HttpTokens")
      Prelude.<*> (x Core..@? "HttpEndpoint")

instance Prelude.Hashable InstanceMetadataOptions where
  hashWithSalt _salt InstanceMetadataOptions' {..} =
    _salt
      `Prelude.hashWithSalt` httpPutResponseHopLimit
      `Prelude.hashWithSalt` httpTokens
      `Prelude.hashWithSalt` httpEndpoint

instance Prelude.NFData InstanceMetadataOptions where
  rnf InstanceMetadataOptions' {..} =
    Prelude.rnf httpPutResponseHopLimit
      `Prelude.seq` Prelude.rnf httpTokens
      `Prelude.seq` Prelude.rnf httpEndpoint

instance Core.ToQuery InstanceMetadataOptions where
  toQuery InstanceMetadataOptions' {..} =
    Prelude.mconcat
      [ "HttpPutResponseHopLimit"
          Core.=: httpPutResponseHopLimit,
        "HttpTokens" Core.=: httpTokens,
        "HttpEndpoint" Core.=: httpEndpoint
      ]
