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
-- Module      : Amazonka.Lambda.Types.SelfManagedEventSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.SelfManagedEventSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.EndPointType
import qualified Amazonka.Prelude as Prelude

-- | The self-managed Apache Kafka cluster for your event source.
--
-- /See:/ 'newSelfManagedEventSource' smart constructor.
data SelfManagedEventSource = SelfManagedEventSource'
  { -- | The list of bootstrap servers for your Kafka brokers in the following
    -- format:
    -- @\"KAFKA_BOOTSTRAP_SERVERS\": [\"abc.xyz.com:xxxx\",\"abc2.xyz.com:xxxx\"]@.
    endpoints :: Prelude.Maybe (Prelude.HashMap EndPointType (Prelude.NonEmpty Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfManagedEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'selfManagedEventSource_endpoints' - The list of bootstrap servers for your Kafka brokers in the following
-- format:
-- @\"KAFKA_BOOTSTRAP_SERVERS\": [\"abc.xyz.com:xxxx\",\"abc2.xyz.com:xxxx\"]@.
newSelfManagedEventSource ::
  SelfManagedEventSource
newSelfManagedEventSource =
  SelfManagedEventSource'
    { endpoints =
        Prelude.Nothing
    }

-- | The list of bootstrap servers for your Kafka brokers in the following
-- format:
-- @\"KAFKA_BOOTSTRAP_SERVERS\": [\"abc.xyz.com:xxxx\",\"abc2.xyz.com:xxxx\"]@.
selfManagedEventSource_endpoints :: Lens.Lens' SelfManagedEventSource (Prelude.Maybe (Prelude.HashMap EndPointType (Prelude.NonEmpty Prelude.Text)))
selfManagedEventSource_endpoints = Lens.lens (\SelfManagedEventSource' {endpoints} -> endpoints) (\s@SelfManagedEventSource' {} a -> s {endpoints = a} :: SelfManagedEventSource) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SelfManagedEventSource where
  parseJSON =
    Data.withObject
      "SelfManagedEventSource"
      ( \x ->
          SelfManagedEventSource'
            Prelude.<$> (x Data..:? "Endpoints" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable SelfManagedEventSource where
  hashWithSalt _salt SelfManagedEventSource' {..} =
    _salt `Prelude.hashWithSalt` endpoints

instance Prelude.NFData SelfManagedEventSource where
  rnf SelfManagedEventSource' {..} =
    Prelude.rnf endpoints

instance Data.ToJSON SelfManagedEventSource where
  toJSON SelfManagedEventSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Endpoints" Data..=) Prelude.<$> endpoints]
      )
