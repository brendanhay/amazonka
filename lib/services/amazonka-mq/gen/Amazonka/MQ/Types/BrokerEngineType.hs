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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.BrokerEngineType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MQ.Types.EngineType
import Amazonka.MQ.Types.EngineVersion
import qualified Amazonka.Prelude as Prelude

-- | Types of broker engines.
--
-- /See:/ 'newBrokerEngineType' smart constructor.
data BrokerEngineType = BrokerEngineType'
  { -- | The list of engine versions.
    engineVersions :: Prelude.Maybe [EngineVersion],
    -- | The broker\'s engine type.
    engineType :: Prelude.Maybe EngineType
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
-- 'engineVersions', 'brokerEngineType_engineVersions' - The list of engine versions.
--
-- 'engineType', 'brokerEngineType_engineType' - The broker\'s engine type.
newBrokerEngineType ::
  BrokerEngineType
newBrokerEngineType =
  BrokerEngineType'
    { engineVersions = Prelude.Nothing,
      engineType = Prelude.Nothing
    }

-- | The list of engine versions.
brokerEngineType_engineVersions :: Lens.Lens' BrokerEngineType (Prelude.Maybe [EngineVersion])
brokerEngineType_engineVersions = Lens.lens (\BrokerEngineType' {engineVersions} -> engineVersions) (\s@BrokerEngineType' {} a -> s {engineVersions = a} :: BrokerEngineType) Prelude.. Lens.mapping Lens.coerced

-- | The broker\'s engine type.
brokerEngineType_engineType :: Lens.Lens' BrokerEngineType (Prelude.Maybe EngineType)
brokerEngineType_engineType = Lens.lens (\BrokerEngineType' {engineType} -> engineType) (\s@BrokerEngineType' {} a -> s {engineType = a} :: BrokerEngineType)

instance Core.FromJSON BrokerEngineType where
  parseJSON =
    Core.withObject
      "BrokerEngineType"
      ( \x ->
          BrokerEngineType'
            Prelude.<$> (x Core..:? "engineVersions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "engineType")
      )

instance Prelude.Hashable BrokerEngineType where
  hashWithSalt _salt BrokerEngineType' {..} =
    _salt `Prelude.hashWithSalt` engineVersions
      `Prelude.hashWithSalt` engineType

instance Prelude.NFData BrokerEngineType where
  rnf BrokerEngineType' {..} =
    Prelude.rnf engineVersions
      `Prelude.seq` Prelude.rnf engineType
