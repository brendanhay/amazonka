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
-- Module      : Amazonka.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.SingleMasterChannelEndpointConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.ChannelProtocol
import Amazonka.KinesisVideo.Types.ChannelRole
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the endpoint configuration for the
-- @SINGLE_MASTER@ channel type.
--
-- /See:/ 'newSingleMasterChannelEndpointConfiguration' smart constructor.
data SingleMasterChannelEndpointConfiguration = SingleMasterChannelEndpointConfiguration'
  { -- | This property is used to determine the nature of communication over this
    -- @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API
    -- returns a websocket endpoint. If @HTTPS@ is specified, this API returns
    -- an @HTTPS@ endpoint.
    protocols :: Prelude.Maybe (Prelude.NonEmpty ChannelProtocol),
    -- | This property is used to determine messaging permissions in this
    -- @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API
    -- returns an endpoint that a client can use to receive offers from and
    -- send answers to any of the viewers on this signaling channel. If
    -- @VIEWER@ is specified, this API returns an endpoint that a client can
    -- use only to send offers to another @MASTER@ client on this signaling
    -- channel.
    role' :: Prelude.Maybe ChannelRole
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SingleMasterChannelEndpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocols', 'singleMasterChannelEndpointConfiguration_protocols' - This property is used to determine the nature of communication over this
-- @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API
-- returns a websocket endpoint. If @HTTPS@ is specified, this API returns
-- an @HTTPS@ endpoint.
--
-- 'role'', 'singleMasterChannelEndpointConfiguration_role' - This property is used to determine messaging permissions in this
-- @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API
-- returns an endpoint that a client can use to receive offers from and
-- send answers to any of the viewers on this signaling channel. If
-- @VIEWER@ is specified, this API returns an endpoint that a client can
-- use only to send offers to another @MASTER@ client on this signaling
-- channel.
newSingleMasterChannelEndpointConfiguration ::
  SingleMasterChannelEndpointConfiguration
newSingleMasterChannelEndpointConfiguration =
  SingleMasterChannelEndpointConfiguration'
    { protocols =
        Prelude.Nothing,
      role' = Prelude.Nothing
    }

-- | This property is used to determine the nature of communication over this
-- @SINGLE_MASTER@ signaling channel. If @WSS@ is specified, this API
-- returns a websocket endpoint. If @HTTPS@ is specified, this API returns
-- an @HTTPS@ endpoint.
singleMasterChannelEndpointConfiguration_protocols :: Lens.Lens' SingleMasterChannelEndpointConfiguration (Prelude.Maybe (Prelude.NonEmpty ChannelProtocol))
singleMasterChannelEndpointConfiguration_protocols = Lens.lens (\SingleMasterChannelEndpointConfiguration' {protocols} -> protocols) (\s@SingleMasterChannelEndpointConfiguration' {} a -> s {protocols = a} :: SingleMasterChannelEndpointConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | This property is used to determine messaging permissions in this
-- @SINGLE_MASTER@ signaling channel. If @MASTER@ is specified, this API
-- returns an endpoint that a client can use to receive offers from and
-- send answers to any of the viewers on this signaling channel. If
-- @VIEWER@ is specified, this API returns an endpoint that a client can
-- use only to send offers to another @MASTER@ client on this signaling
-- channel.
singleMasterChannelEndpointConfiguration_role :: Lens.Lens' SingleMasterChannelEndpointConfiguration (Prelude.Maybe ChannelRole)
singleMasterChannelEndpointConfiguration_role = Lens.lens (\SingleMasterChannelEndpointConfiguration' {role'} -> role') (\s@SingleMasterChannelEndpointConfiguration' {} a -> s {role' = a} :: SingleMasterChannelEndpointConfiguration)

instance
  Prelude.Hashable
    SingleMasterChannelEndpointConfiguration
  where
  hashWithSalt
    _salt
    SingleMasterChannelEndpointConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` protocols
        `Prelude.hashWithSalt` role'

instance
  Prelude.NFData
    SingleMasterChannelEndpointConfiguration
  where
  rnf SingleMasterChannelEndpointConfiguration' {..} =
    Prelude.rnf protocols `Prelude.seq`
      Prelude.rnf role'

instance
  Data.ToJSON
    SingleMasterChannelEndpointConfiguration
  where
  toJSON SingleMasterChannelEndpointConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Protocols" Data..=) Prelude.<$> protocols,
            ("Role" Data..=) Prelude.<$> role'
          ]
      )
