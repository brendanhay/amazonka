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
-- Module      : Amazonka.MediaConnect.Types.ListedGateway
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.ListedGateway where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.GatewayState
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of a gateway, including its name, ARN, and status.
--
-- /See:/ 'newListedGateway' smart constructor.
data ListedGateway = ListedGateway'
  { -- | The Amazon Resource Name (ARN) of the gateway.
    gatewayArn :: Prelude.Text,
    gatewayState :: GatewayState,
    -- | The name of the gateway.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'listedGateway_gatewayArn' - The Amazon Resource Name (ARN) of the gateway.
--
-- 'gatewayState', 'listedGateway_gatewayState' - Undocumented member.
--
-- 'name', 'listedGateway_name' - The name of the gateway.
newListedGateway ::
  -- | 'gatewayArn'
  Prelude.Text ->
  -- | 'gatewayState'
  GatewayState ->
  -- | 'name'
  Prelude.Text ->
  ListedGateway
newListedGateway pGatewayArn_ pGatewayState_ pName_ =
  ListedGateway'
    { gatewayArn = pGatewayArn_,
      gatewayState = pGatewayState_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of the gateway.
listedGateway_gatewayArn :: Lens.Lens' ListedGateway Prelude.Text
listedGateway_gatewayArn = Lens.lens (\ListedGateway' {gatewayArn} -> gatewayArn) (\s@ListedGateway' {} a -> s {gatewayArn = a} :: ListedGateway)

-- | Undocumented member.
listedGateway_gatewayState :: Lens.Lens' ListedGateway GatewayState
listedGateway_gatewayState = Lens.lens (\ListedGateway' {gatewayState} -> gatewayState) (\s@ListedGateway' {} a -> s {gatewayState = a} :: ListedGateway)

-- | The name of the gateway.
listedGateway_name :: Lens.Lens' ListedGateway Prelude.Text
listedGateway_name = Lens.lens (\ListedGateway' {name} -> name) (\s@ListedGateway' {} a -> s {name = a} :: ListedGateway)

instance Data.FromJSON ListedGateway where
  parseJSON =
    Data.withObject
      "ListedGateway"
      ( \x ->
          ListedGateway'
            Prelude.<$> (x Data..: "gatewayArn")
            Prelude.<*> (x Data..: "gatewayState")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ListedGateway where
  hashWithSalt _salt ListedGateway' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayArn
      `Prelude.hashWithSalt` gatewayState
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListedGateway where
  rnf ListedGateway' {..} =
    Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf gatewayState
      `Prelude.seq` Prelude.rnf name
