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
-- Module      : Amazonka.Kafka.Types.ServerlessClientAuthentication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ServerlessClientAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.ServerlessSasl
import qualified Amazonka.Prelude as Prelude

-- | Includes all client authentication information.
--
-- /See:/ 'newServerlessClientAuthentication' smart constructor.
data ServerlessClientAuthentication = ServerlessClientAuthentication'
  { -- | Details for ClientAuthentication using SASL.
    sasl :: Prelude.Maybe ServerlessSasl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerlessClientAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sasl', 'serverlessClientAuthentication_sasl' - Details for ClientAuthentication using SASL.
newServerlessClientAuthentication ::
  ServerlessClientAuthentication
newServerlessClientAuthentication =
  ServerlessClientAuthentication'
    { sasl =
        Prelude.Nothing
    }

-- | Details for ClientAuthentication using SASL.
serverlessClientAuthentication_sasl :: Lens.Lens' ServerlessClientAuthentication (Prelude.Maybe ServerlessSasl)
serverlessClientAuthentication_sasl = Lens.lens (\ServerlessClientAuthentication' {sasl} -> sasl) (\s@ServerlessClientAuthentication' {} a -> s {sasl = a} :: ServerlessClientAuthentication)

instance Data.FromJSON ServerlessClientAuthentication where
  parseJSON =
    Data.withObject
      "ServerlessClientAuthentication"
      ( \x ->
          ServerlessClientAuthentication'
            Prelude.<$> (x Data..:? "sasl")
      )

instance
  Prelude.Hashable
    ServerlessClientAuthentication
  where
  hashWithSalt
    _salt
    ServerlessClientAuthentication' {..} =
      _salt `Prelude.hashWithSalt` sasl

instance
  Prelude.NFData
    ServerlessClientAuthentication
  where
  rnf ServerlessClientAuthentication' {..} =
    Prelude.rnf sasl

instance Data.ToJSON ServerlessClientAuthentication where
  toJSON ServerlessClientAuthentication' {..} =
    Data.object
      ( Prelude.catMaybes
          [("sasl" Data..=) Prelude.<$> sasl]
      )
