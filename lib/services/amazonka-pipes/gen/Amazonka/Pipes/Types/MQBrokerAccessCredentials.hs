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
-- Module      : Amazonka.Pipes.Types.MQBrokerAccessCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.MQBrokerAccessCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Secrets Manager secret that stores your broker credentials.
--
-- /See:/ 'newMQBrokerAccessCredentials' smart constructor.
data MQBrokerAccessCredentials = MQBrokerAccessCredentials'
  { -- | The ARN of the Secrets Manager secret.
    basicAuth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MQBrokerAccessCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basicAuth', 'mQBrokerAccessCredentials_basicAuth' - The ARN of the Secrets Manager secret.
newMQBrokerAccessCredentials ::
  MQBrokerAccessCredentials
newMQBrokerAccessCredentials =
  MQBrokerAccessCredentials'
    { basicAuth =
        Prelude.Nothing
    }

-- | The ARN of the Secrets Manager secret.
mQBrokerAccessCredentials_basicAuth :: Lens.Lens' MQBrokerAccessCredentials (Prelude.Maybe Prelude.Text)
mQBrokerAccessCredentials_basicAuth = Lens.lens (\MQBrokerAccessCredentials' {basicAuth} -> basicAuth) (\s@MQBrokerAccessCredentials' {} a -> s {basicAuth = a} :: MQBrokerAccessCredentials)

instance Data.FromJSON MQBrokerAccessCredentials where
  parseJSON =
    Data.withObject
      "MQBrokerAccessCredentials"
      ( \x ->
          MQBrokerAccessCredentials'
            Prelude.<$> (x Data..:? "BasicAuth")
      )

instance Prelude.Hashable MQBrokerAccessCredentials where
  hashWithSalt _salt MQBrokerAccessCredentials' {..} =
    _salt `Prelude.hashWithSalt` basicAuth

instance Prelude.NFData MQBrokerAccessCredentials where
  rnf MQBrokerAccessCredentials' {..} =
    Prelude.rnf basicAuth

instance Data.ToJSON MQBrokerAccessCredentials where
  toJSON MQBrokerAccessCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [("BasicAuth" Data..=) Prelude.<$> basicAuth]
      )
