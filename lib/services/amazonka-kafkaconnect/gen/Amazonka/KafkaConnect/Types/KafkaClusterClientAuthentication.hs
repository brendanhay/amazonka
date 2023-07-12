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
-- Module      : Amazonka.KafkaConnect.Types.KafkaClusterClientAuthentication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.KafkaClusterClientAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.KafkaClusterClientAuthenticationType
import qualified Amazonka.Prelude as Prelude

-- | The client authentication information used in order to authenticate with
-- the Apache Kafka cluster.
--
-- /See:/ 'newKafkaClusterClientAuthentication' smart constructor.
data KafkaClusterClientAuthentication = KafkaClusterClientAuthentication'
  { -- | The type of client authentication used to connect to the Apache Kafka
    -- cluster. Value NONE means that no client authentication is used.
    authenticationType :: KafkaClusterClientAuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KafkaClusterClientAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationType', 'kafkaClusterClientAuthentication_authenticationType' - The type of client authentication used to connect to the Apache Kafka
-- cluster. Value NONE means that no client authentication is used.
newKafkaClusterClientAuthentication ::
  -- | 'authenticationType'
  KafkaClusterClientAuthenticationType ->
  KafkaClusterClientAuthentication
newKafkaClusterClientAuthentication
  pAuthenticationType_ =
    KafkaClusterClientAuthentication'
      { authenticationType =
          pAuthenticationType_
      }

-- | The type of client authentication used to connect to the Apache Kafka
-- cluster. Value NONE means that no client authentication is used.
kafkaClusterClientAuthentication_authenticationType :: Lens.Lens' KafkaClusterClientAuthentication KafkaClusterClientAuthenticationType
kafkaClusterClientAuthentication_authenticationType = Lens.lens (\KafkaClusterClientAuthentication' {authenticationType} -> authenticationType) (\s@KafkaClusterClientAuthentication' {} a -> s {authenticationType = a} :: KafkaClusterClientAuthentication)

instance
  Prelude.Hashable
    KafkaClusterClientAuthentication
  where
  hashWithSalt
    _salt
    KafkaClusterClientAuthentication' {..} =
      _salt `Prelude.hashWithSalt` authenticationType

instance
  Prelude.NFData
    KafkaClusterClientAuthentication
  where
  rnf KafkaClusterClientAuthentication' {..} =
    Prelude.rnf authenticationType

instance Data.ToJSON KafkaClusterClientAuthentication where
  toJSON KafkaClusterClientAuthentication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("authenticationType" Data..= authenticationType)
          ]
      )
