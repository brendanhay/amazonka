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
-- Module      : Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KafkaConnect.Types.KafkaClusterEncryptionInTransitType
import qualified Amazonka.Prelude as Prelude

-- | The description of the encryption in transit to the Apache Kafka
-- cluster.
--
-- /See:/ 'newKafkaClusterEncryptionInTransitDescription' smart constructor.
data KafkaClusterEncryptionInTransitDescription = KafkaClusterEncryptionInTransitDescription'
  { -- | The type of encryption in transit to the Apache Kafka cluster.
    encryptionType :: Prelude.Maybe KafkaClusterEncryptionInTransitType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KafkaClusterEncryptionInTransitDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionType', 'kafkaClusterEncryptionInTransitDescription_encryptionType' - The type of encryption in transit to the Apache Kafka cluster.
newKafkaClusterEncryptionInTransitDescription ::
  KafkaClusterEncryptionInTransitDescription
newKafkaClusterEncryptionInTransitDescription =
  KafkaClusterEncryptionInTransitDescription'
    { encryptionType =
        Prelude.Nothing
    }

-- | The type of encryption in transit to the Apache Kafka cluster.
kafkaClusterEncryptionInTransitDescription_encryptionType :: Lens.Lens' KafkaClusterEncryptionInTransitDescription (Prelude.Maybe KafkaClusterEncryptionInTransitType)
kafkaClusterEncryptionInTransitDescription_encryptionType = Lens.lens (\KafkaClusterEncryptionInTransitDescription' {encryptionType} -> encryptionType) (\s@KafkaClusterEncryptionInTransitDescription' {} a -> s {encryptionType = a} :: KafkaClusterEncryptionInTransitDescription)

instance
  Data.FromJSON
    KafkaClusterEncryptionInTransitDescription
  where
  parseJSON =
    Data.withObject
      "KafkaClusterEncryptionInTransitDescription"
      ( \x ->
          KafkaClusterEncryptionInTransitDescription'
            Prelude.<$> (x Data..:? "encryptionType")
      )

instance
  Prelude.Hashable
    KafkaClusterEncryptionInTransitDescription
  where
  hashWithSalt
    _salt
    KafkaClusterEncryptionInTransitDescription' {..} =
      _salt `Prelude.hashWithSalt` encryptionType

instance
  Prelude.NFData
    KafkaClusterEncryptionInTransitDescription
  where
  rnf KafkaClusterEncryptionInTransitDescription' {..} =
    Prelude.rnf encryptionType
