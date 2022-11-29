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
-- Module      : Amazonka.Kendra.Types.ServerSideEncryptionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ServerSideEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the identifier of the KMS key used to encrypt data indexed by
-- Amazon Kendra. Amazon Kendra doesn\'t support asymmetric keys.
--
-- /See:/ 'newServerSideEncryptionConfiguration' smart constructor.
data ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { -- | The identifier of the KMS key. Amazon Kendra doesn\'t support asymmetric
    -- keys.
    kmsKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'serverSideEncryptionConfiguration_kmsKeyId' - The identifier of the KMS key. Amazon Kendra doesn\'t support asymmetric
-- keys.
newServerSideEncryptionConfiguration ::
  ServerSideEncryptionConfiguration
newServerSideEncryptionConfiguration =
  ServerSideEncryptionConfiguration'
    { kmsKeyId =
        Prelude.Nothing
    }

-- | The identifier of the KMS key. Amazon Kendra doesn\'t support asymmetric
-- keys.
serverSideEncryptionConfiguration_kmsKeyId :: Lens.Lens' ServerSideEncryptionConfiguration (Prelude.Maybe Prelude.Text)
serverSideEncryptionConfiguration_kmsKeyId = Lens.lens (\ServerSideEncryptionConfiguration' {kmsKeyId} -> kmsKeyId) (\s@ServerSideEncryptionConfiguration' {} a -> s {kmsKeyId = a} :: ServerSideEncryptionConfiguration) Prelude.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    ServerSideEncryptionConfiguration
  where
  parseJSON =
    Core.withObject
      "ServerSideEncryptionConfiguration"
      ( \x ->
          ServerSideEncryptionConfiguration'
            Prelude.<$> (x Core..:? "KmsKeyId")
      )

instance
  Prelude.Hashable
    ServerSideEncryptionConfiguration
  where
  hashWithSalt
    _salt
    ServerSideEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` kmsKeyId

instance
  Prelude.NFData
    ServerSideEncryptionConfiguration
  where
  rnf ServerSideEncryptionConfiguration' {..} =
    Prelude.rnf kmsKeyId

instance
  Core.ToJSON
    ServerSideEncryptionConfiguration
  where
  toJSON ServerSideEncryptionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("KmsKeyId" Core..=) Prelude.<$> kmsKeyId]
      )
