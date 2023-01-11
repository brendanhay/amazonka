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
-- Module      : Amazonka.MQ.Types.BrokerEngineType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.BrokerEngineType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types.EngineType
import Amazonka.MQ.Types.EngineVersion
import qualified Amazonka.Prelude as Prelude

-- | Types of broker engines.
--
-- /See:/ 'newBrokerEngineType' smart constructor.
data BrokerEngineType = BrokerEngineType'
  { -- | The broker\'s engine type.
    engineType :: Prelude.Maybe EngineType,
    -- | The list of engine versions.
    engineVersions :: Prelude.Maybe [EngineVersion]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerEngineType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineType', 'brokerEngineType_engineType' - The broker\'s engine type.
--
-- 'engineVersions', 'brokerEngineType_engineVersions' - The list of engine versions.
newBrokerEngineType ::
  BrokerEngineType
newBrokerEngineType =
  BrokerEngineType'
    { engineType = Prelude.Nothing,
      engineVersions = Prelude.Nothing
    }

-- | The broker\'s engine type.
brokerEngineType_engineType :: Lens.Lens' BrokerEngineType (Prelude.Maybe EngineType)
brokerEngineType_engineType = Lens.lens (\BrokerEngineType' {engineType} -> engineType) (\s@BrokerEngineType' {} a -> s {engineType = a} :: BrokerEngineType)

-- | The list of engine versions.
brokerEngineType_engineVersions :: Lens.Lens' BrokerEngineType (Prelude.Maybe [EngineVersion])
brokerEngineType_engineVersions = Lens.lens (\BrokerEngineType' {engineVersions} -> engineVersions) (\s@BrokerEngineType' {} a -> s {engineVersions = a} :: BrokerEngineType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BrokerEngineType where
  parseJSON =
    Data.withObject
      "BrokerEngineType"
      ( \x ->
          BrokerEngineType'
            Prelude.<$> (x Data..:? "engineType")
            Prelude.<*> ( x Data..:? "engineVersions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BrokerEngineType where
  hashWithSalt _salt BrokerEngineType' {..} =
    _salt `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` engineVersions

instance Prelude.NFData BrokerEngineType where
  rnf BrokerEngineType' {..} =
    Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf engineVersions
