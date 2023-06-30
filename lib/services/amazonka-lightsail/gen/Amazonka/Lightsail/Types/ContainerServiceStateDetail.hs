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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceStateDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceStateDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.ContainerServiceStateDetailCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the current state of a container service.
--
-- /See:/ 'newContainerServiceStateDetail' smart constructor.
data ContainerServiceStateDetail = ContainerServiceStateDetail'
  { -- | The state code of the container service.
    --
    -- The following state codes are possible:
    --
    -- -   The following state codes are possible if your container service is
    --     in a @DEPLOYING@ or @UPDATING@ state:
    --
    --     -   @CREATING_SYSTEM_RESOURCES@ - The system resources for your
    --         container service are being created.
    --
    --     -   @CREATING_NETWORK_INFRASTRUCTURE@ - The network infrastructure
    --         for your container service are being created.
    --
    --     -   @PROVISIONING_CERTIFICATE@ - The SSL\/TLS certificate for your
    --         container service is being created.
    --
    --     -   @PROVISIONING_SERVICE@ - Your container service is being
    --         provisioned.
    --
    --     -   @CREATING_DEPLOYMENT@ - Your deployment is being created on your
    --         container service.
    --
    --     -   @EVALUATING_HEALTH_CHECK@ - The health of your deployment is
    --         being evaluated.
    --
    --     -   @ACTIVATING_DEPLOYMENT@ - Your deployment is being activated.
    --
    -- -   The following state codes are possible if your container service is
    --     in a @PENDING@ state:
    --
    --     -   @CERTIFICATE_LIMIT_EXCEEDED@ - The SSL\/TLS certificate required
    --         for your container service exceeds the maximum number of
    --         certificates allowed for your account.
    --
    --     -   @UNKNOWN_ERROR@ - An error was experienced when your container
    --         service was being created.
    code :: Prelude.Maybe ContainerServiceStateDetailCode,
    -- | A message that provides more information for the state code.
    --
    -- The state detail is populated only when a container service is in a
    -- @PENDING@, @DEPLOYING@, or @UPDATING@ state.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceStateDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'containerServiceStateDetail_code' - The state code of the container service.
--
-- The following state codes are possible:
--
-- -   The following state codes are possible if your container service is
--     in a @DEPLOYING@ or @UPDATING@ state:
--
--     -   @CREATING_SYSTEM_RESOURCES@ - The system resources for your
--         container service are being created.
--
--     -   @CREATING_NETWORK_INFRASTRUCTURE@ - The network infrastructure
--         for your container service are being created.
--
--     -   @PROVISIONING_CERTIFICATE@ - The SSL\/TLS certificate for your
--         container service is being created.
--
--     -   @PROVISIONING_SERVICE@ - Your container service is being
--         provisioned.
--
--     -   @CREATING_DEPLOYMENT@ - Your deployment is being created on your
--         container service.
--
--     -   @EVALUATING_HEALTH_CHECK@ - The health of your deployment is
--         being evaluated.
--
--     -   @ACTIVATING_DEPLOYMENT@ - Your deployment is being activated.
--
-- -   The following state codes are possible if your container service is
--     in a @PENDING@ state:
--
--     -   @CERTIFICATE_LIMIT_EXCEEDED@ - The SSL\/TLS certificate required
--         for your container service exceeds the maximum number of
--         certificates allowed for your account.
--
--     -   @UNKNOWN_ERROR@ - An error was experienced when your container
--         service was being created.
--
-- 'message', 'containerServiceStateDetail_message' - A message that provides more information for the state code.
--
-- The state detail is populated only when a container service is in a
-- @PENDING@, @DEPLOYING@, or @UPDATING@ state.
newContainerServiceStateDetail ::
  ContainerServiceStateDetail
newContainerServiceStateDetail =
  ContainerServiceStateDetail'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The state code of the container service.
--
-- The following state codes are possible:
--
-- -   The following state codes are possible if your container service is
--     in a @DEPLOYING@ or @UPDATING@ state:
--
--     -   @CREATING_SYSTEM_RESOURCES@ - The system resources for your
--         container service are being created.
--
--     -   @CREATING_NETWORK_INFRASTRUCTURE@ - The network infrastructure
--         for your container service are being created.
--
--     -   @PROVISIONING_CERTIFICATE@ - The SSL\/TLS certificate for your
--         container service is being created.
--
--     -   @PROVISIONING_SERVICE@ - Your container service is being
--         provisioned.
--
--     -   @CREATING_DEPLOYMENT@ - Your deployment is being created on your
--         container service.
--
--     -   @EVALUATING_HEALTH_CHECK@ - The health of your deployment is
--         being evaluated.
--
--     -   @ACTIVATING_DEPLOYMENT@ - Your deployment is being activated.
--
-- -   The following state codes are possible if your container service is
--     in a @PENDING@ state:
--
--     -   @CERTIFICATE_LIMIT_EXCEEDED@ - The SSL\/TLS certificate required
--         for your container service exceeds the maximum number of
--         certificates allowed for your account.
--
--     -   @UNKNOWN_ERROR@ - An error was experienced when your container
--         service was being created.
containerServiceStateDetail_code :: Lens.Lens' ContainerServiceStateDetail (Prelude.Maybe ContainerServiceStateDetailCode)
containerServiceStateDetail_code = Lens.lens (\ContainerServiceStateDetail' {code} -> code) (\s@ContainerServiceStateDetail' {} a -> s {code = a} :: ContainerServiceStateDetail)

-- | A message that provides more information for the state code.
--
-- The state detail is populated only when a container service is in a
-- @PENDING@, @DEPLOYING@, or @UPDATING@ state.
containerServiceStateDetail_message :: Lens.Lens' ContainerServiceStateDetail (Prelude.Maybe Prelude.Text)
containerServiceStateDetail_message = Lens.lens (\ContainerServiceStateDetail' {message} -> message) (\s@ContainerServiceStateDetail' {} a -> s {message = a} :: ContainerServiceStateDetail)

instance Data.FromJSON ContainerServiceStateDetail where
  parseJSON =
    Data.withObject
      "ContainerServiceStateDetail"
      ( \x ->
          ContainerServiceStateDetail'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
      )

instance Prelude.Hashable ContainerServiceStateDetail where
  hashWithSalt _salt ContainerServiceStateDetail' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message

instance Prelude.NFData ContainerServiceStateDetail where
  rnf ContainerServiceStateDetail' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
