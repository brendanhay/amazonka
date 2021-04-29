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
-- Module      : Network.AWS.MQ.Types.BrokerEngineType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerEngineType where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.EngineType
import Network.AWS.MQ.Types.EngineVersion
import qualified Network.AWS.Prelude as Prelude

-- | Types of broker engines.
--
-- /See:/ 'newBrokerEngineType' smart constructor.
data BrokerEngineType = BrokerEngineType'
  { -- | The type of broker engine.
    engineType :: Prelude.Maybe EngineType,
    -- | The list of engine versions.
    engineVersions :: Prelude.Maybe [EngineVersion]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BrokerEngineType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineType', 'brokerEngineType_engineType' - The type of broker engine.
--
-- 'engineVersions', 'brokerEngineType_engineVersions' - The list of engine versions.
newBrokerEngineType ::
  BrokerEngineType
newBrokerEngineType =
  BrokerEngineType'
    { engineType = Prelude.Nothing,
      engineVersions = Prelude.Nothing
    }

-- | The type of broker engine.
brokerEngineType_engineType :: Lens.Lens' BrokerEngineType (Prelude.Maybe EngineType)
brokerEngineType_engineType = Lens.lens (\BrokerEngineType' {engineType} -> engineType) (\s@BrokerEngineType' {} a -> s {engineType = a} :: BrokerEngineType)

-- | The list of engine versions.
brokerEngineType_engineVersions :: Lens.Lens' BrokerEngineType (Prelude.Maybe [EngineVersion])
brokerEngineType_engineVersions = Lens.lens (\BrokerEngineType' {engineVersions} -> engineVersions) (\s@BrokerEngineType' {} a -> s {engineVersions = a} :: BrokerEngineType) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON BrokerEngineType where
  parseJSON =
    Prelude.withObject
      "BrokerEngineType"
      ( \x ->
          BrokerEngineType'
            Prelude.<$> (x Prelude..:? "engineType")
            Prelude.<*> ( x Prelude..:? "engineVersions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BrokerEngineType

instance Prelude.NFData BrokerEngineType
