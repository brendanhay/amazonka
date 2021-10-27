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
-- Module      : Network.AWS.KafkaConnect.Types.KafkaClusterEncryptionInTransitDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KafkaConnect.Types.KafkaClusterEncryptionInTransitDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.KafkaConnect.Types.KafkaClusterEncryptionInTransitType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  Core.FromJSON
    KafkaClusterEncryptionInTransitDescription
  where
  parseJSON =
    Core.withObject
      "KafkaClusterEncryptionInTransitDescription"
      ( \x ->
          KafkaClusterEncryptionInTransitDescription'
            Prelude.<$> (x Core..:? "encryptionType")
      )

instance
  Prelude.Hashable
    KafkaClusterEncryptionInTransitDescription

instance
  Prelude.NFData
    KafkaClusterEncryptionInTransitDescription
