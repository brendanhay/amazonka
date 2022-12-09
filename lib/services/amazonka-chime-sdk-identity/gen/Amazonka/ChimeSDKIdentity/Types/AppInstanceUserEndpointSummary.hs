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
-- Module      : Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointSummary where

import Amazonka.ChimeSDKIdentity.Types.AllowMessages
import Amazonka.ChimeSDKIdentity.Types.AppInstanceUserEndpointType
import Amazonka.ChimeSDKIdentity.Types.EndpointState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the details of an @AppInstanceUserEndpoint@.
--
-- /See:/ 'newAppInstanceUserEndpointSummary' smart constructor.
data AppInstanceUserEndpointSummary = AppInstanceUserEndpointSummary'
  { -- | BBoolean that controls whether the @AppInstanceUserEndpoint@ is opted in
    -- to receive messages. @ALL@ indicates the endpoint will receive all
    -- messages. @NONE@ indicates the endpoint will receive no messages.
    allowMessages :: Prelude.Maybe AllowMessages,
    -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The unique identifier of the @AppInstanceUserEndpoint@.
    endpointId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A read-only field that represent the state of an
    -- @AppInstanceUserEndpoint@.
    endpointState :: Prelude.Maybe EndpointState,
    -- | The name of the @AppInstanceUserEndpoint@.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The type of the @AppInstanceUserEndpoint@.
    type' :: Prelude.Maybe AppInstanceUserEndpointType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppInstanceUserEndpointSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowMessages', 'appInstanceUserEndpointSummary_allowMessages' - BBoolean that controls whether the @AppInstanceUserEndpoint@ is opted in
-- to receive messages. @ALL@ indicates the endpoint will receive all
-- messages. @NONE@ indicates the endpoint will receive no messages.
--
-- 'appInstanceUserArn', 'appInstanceUserEndpointSummary_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'endpointId', 'appInstanceUserEndpointSummary_endpointId' - The unique identifier of the @AppInstanceUserEndpoint@.
--
-- 'endpointState', 'appInstanceUserEndpointSummary_endpointState' - A read-only field that represent the state of an
-- @AppInstanceUserEndpoint@.
--
-- 'name', 'appInstanceUserEndpointSummary_name' - The name of the @AppInstanceUserEndpoint@.
--
-- 'type'', 'appInstanceUserEndpointSummary_type' - The type of the @AppInstanceUserEndpoint@.
newAppInstanceUserEndpointSummary ::
  AppInstanceUserEndpointSummary
newAppInstanceUserEndpointSummary =
  AppInstanceUserEndpointSummary'
    { allowMessages =
        Prelude.Nothing,
      appInstanceUserArn = Prelude.Nothing,
      endpointId = Prelude.Nothing,
      endpointState = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | BBoolean that controls whether the @AppInstanceUserEndpoint@ is opted in
-- to receive messages. @ALL@ indicates the endpoint will receive all
-- messages. @NONE@ indicates the endpoint will receive no messages.
appInstanceUserEndpointSummary_allowMessages :: Lens.Lens' AppInstanceUserEndpointSummary (Prelude.Maybe AllowMessages)
appInstanceUserEndpointSummary_allowMessages = Lens.lens (\AppInstanceUserEndpointSummary' {allowMessages} -> allowMessages) (\s@AppInstanceUserEndpointSummary' {} a -> s {allowMessages = a} :: AppInstanceUserEndpointSummary)

-- | The ARN of the @AppInstanceUser@.
appInstanceUserEndpointSummary_appInstanceUserArn :: Lens.Lens' AppInstanceUserEndpointSummary (Prelude.Maybe Prelude.Text)
appInstanceUserEndpointSummary_appInstanceUserArn = Lens.lens (\AppInstanceUserEndpointSummary' {appInstanceUserArn} -> appInstanceUserArn) (\s@AppInstanceUserEndpointSummary' {} a -> s {appInstanceUserArn = a} :: AppInstanceUserEndpointSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The unique identifier of the @AppInstanceUserEndpoint@.
appInstanceUserEndpointSummary_endpointId :: Lens.Lens' AppInstanceUserEndpointSummary (Prelude.Maybe Prelude.Text)
appInstanceUserEndpointSummary_endpointId = Lens.lens (\AppInstanceUserEndpointSummary' {endpointId} -> endpointId) (\s@AppInstanceUserEndpointSummary' {} a -> s {endpointId = a} :: AppInstanceUserEndpointSummary) Prelude.. Lens.mapping Data._Sensitive

-- | A read-only field that represent the state of an
-- @AppInstanceUserEndpoint@.
appInstanceUserEndpointSummary_endpointState :: Lens.Lens' AppInstanceUserEndpointSummary (Prelude.Maybe EndpointState)
appInstanceUserEndpointSummary_endpointState = Lens.lens (\AppInstanceUserEndpointSummary' {endpointState} -> endpointState) (\s@AppInstanceUserEndpointSummary' {} a -> s {endpointState = a} :: AppInstanceUserEndpointSummary)

-- | The name of the @AppInstanceUserEndpoint@.
appInstanceUserEndpointSummary_name :: Lens.Lens' AppInstanceUserEndpointSummary (Prelude.Maybe Prelude.Text)
appInstanceUserEndpointSummary_name = Lens.lens (\AppInstanceUserEndpointSummary' {name} -> name) (\s@AppInstanceUserEndpointSummary' {} a -> s {name = a} :: AppInstanceUserEndpointSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The type of the @AppInstanceUserEndpoint@.
appInstanceUserEndpointSummary_type :: Lens.Lens' AppInstanceUserEndpointSummary (Prelude.Maybe AppInstanceUserEndpointType)
appInstanceUserEndpointSummary_type = Lens.lens (\AppInstanceUserEndpointSummary' {type'} -> type') (\s@AppInstanceUserEndpointSummary' {} a -> s {type' = a} :: AppInstanceUserEndpointSummary)

instance Data.FromJSON AppInstanceUserEndpointSummary where
  parseJSON =
    Data.withObject
      "AppInstanceUserEndpointSummary"
      ( \x ->
          AppInstanceUserEndpointSummary'
            Prelude.<$> (x Data..:? "AllowMessages")
            Prelude.<*> (x Data..:? "AppInstanceUserArn")
            Prelude.<*> (x Data..:? "EndpointId")
            Prelude.<*> (x Data..:? "EndpointState")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AppInstanceUserEndpointSummary
  where
  hashWithSalt
    _salt
    AppInstanceUserEndpointSummary' {..} =
      _salt `Prelude.hashWithSalt` allowMessages
        `Prelude.hashWithSalt` appInstanceUserArn
        `Prelude.hashWithSalt` endpointId
        `Prelude.hashWithSalt` endpointState
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AppInstanceUserEndpointSummary
  where
  rnf AppInstanceUserEndpointSummary' {..} =
    Prelude.rnf allowMessages
      `Prelude.seq` Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf endpointState
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
