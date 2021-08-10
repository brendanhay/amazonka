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
-- Module      : Network.AWS.Lambda.Types.SelfManagedEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.SelfManagedEventSource where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.EndPointType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Self-Managed Apache Kafka cluster for your event source.
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
selfManagedEventSource_endpoints = Lens.lens (\SelfManagedEventSource' {endpoints} -> endpoints) (\s@SelfManagedEventSource' {} a -> s {endpoints = a} :: SelfManagedEventSource) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON SelfManagedEventSource where
  parseJSON =
    Core.withObject
      "SelfManagedEventSource"
      ( \x ->
          SelfManagedEventSource'
            Prelude.<$> (x Core..:? "Endpoints" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SelfManagedEventSource

instance Prelude.NFData SelfManagedEventSource

instance Core.ToJSON SelfManagedEventSource where
  toJSON SelfManagedEventSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Endpoints" Core..=) Prelude.<$> endpoints]
      )
