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
-- Module      : Amazonka.ElasticSearch.Types.EncryptionAtRestOptionsStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.EncryptionAtRestOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.EncryptionAtRestOptions
import Amazonka.ElasticSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | Status of the Encryption At Rest options for the specified Elasticsearch
-- domain.
--
-- /See:/ 'newEncryptionAtRestOptionsStatus' smart constructor.
data EncryptionAtRestOptionsStatus = EncryptionAtRestOptionsStatus'
  { -- | Specifies the Encryption At Rest options for the specified Elasticsearch
    -- domain.
    options :: EncryptionAtRestOptions,
    -- | Specifies the status of the Encryption At Rest options for the specified
    -- Elasticsearch domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionAtRestOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'encryptionAtRestOptionsStatus_options' - Specifies the Encryption At Rest options for the specified Elasticsearch
-- domain.
--
-- 'status', 'encryptionAtRestOptionsStatus_status' - Specifies the status of the Encryption At Rest options for the specified
-- Elasticsearch domain.
newEncryptionAtRestOptionsStatus ::
  -- | 'options'
  EncryptionAtRestOptions ->
  -- | 'status'
  OptionStatus ->
  EncryptionAtRestOptionsStatus
newEncryptionAtRestOptionsStatus pOptions_ pStatus_ =
  EncryptionAtRestOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Specifies the Encryption At Rest options for the specified Elasticsearch
-- domain.
encryptionAtRestOptionsStatus_options :: Lens.Lens' EncryptionAtRestOptionsStatus EncryptionAtRestOptions
encryptionAtRestOptionsStatus_options = Lens.lens (\EncryptionAtRestOptionsStatus' {options} -> options) (\s@EncryptionAtRestOptionsStatus' {} a -> s {options = a} :: EncryptionAtRestOptionsStatus)

-- | Specifies the status of the Encryption At Rest options for the specified
-- Elasticsearch domain.
encryptionAtRestOptionsStatus_status :: Lens.Lens' EncryptionAtRestOptionsStatus OptionStatus
encryptionAtRestOptionsStatus_status = Lens.lens (\EncryptionAtRestOptionsStatus' {status} -> status) (\s@EncryptionAtRestOptionsStatus' {} a -> s {status = a} :: EncryptionAtRestOptionsStatus)

instance Data.FromJSON EncryptionAtRestOptionsStatus where
  parseJSON =
    Data.withObject
      "EncryptionAtRestOptionsStatus"
      ( \x ->
          EncryptionAtRestOptionsStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    EncryptionAtRestOptionsStatus
  where
  hashWithSalt _salt EncryptionAtRestOptionsStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData EncryptionAtRestOptionsStatus where
  rnf EncryptionAtRestOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
