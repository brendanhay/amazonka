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
-- Module      : Amazonka.Kafka.Types.KafkaVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.KafkaVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.KafkaVersionStatus
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newKafkaVersion' smart constructor.
data KafkaVersion = KafkaVersion'
  { status :: Prelude.Maybe KafkaVersionStatus,
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KafkaVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'kafkaVersion_status' - Undocumented member.
--
-- 'version', 'kafkaVersion_version' - Undocumented member.
newKafkaVersion ::
  KafkaVersion
newKafkaVersion =
  KafkaVersion'
    { status = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | Undocumented member.
kafkaVersion_status :: Lens.Lens' KafkaVersion (Prelude.Maybe KafkaVersionStatus)
kafkaVersion_status = Lens.lens (\KafkaVersion' {status} -> status) (\s@KafkaVersion' {} a -> s {status = a} :: KafkaVersion)

-- | Undocumented member.
kafkaVersion_version :: Lens.Lens' KafkaVersion (Prelude.Maybe Prelude.Text)
kafkaVersion_version = Lens.lens (\KafkaVersion' {version} -> version) (\s@KafkaVersion' {} a -> s {version = a} :: KafkaVersion)

instance Data.FromJSON KafkaVersion where
  parseJSON =
    Data.withObject
      "KafkaVersion"
      ( \x ->
          KafkaVersion'
            Prelude.<$> (x Data..:? "status")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable KafkaVersion where
  hashWithSalt _salt KafkaVersion' {..} =
    _salt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` version

instance Prelude.NFData KafkaVersion where
  rnf KafkaVersion' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf version
