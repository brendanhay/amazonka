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
-- Module      : Amazonka.OpenSearch.Types.EncryptionAtRestOptionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.EncryptionAtRestOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.EncryptionAtRestOptions
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | Status of the encryption at rest options for the specified OpenSearch
-- Service domain.
--
-- /See:/ 'newEncryptionAtRestOptionsStatus' smart constructor.
data EncryptionAtRestOptionsStatus = EncryptionAtRestOptionsStatus'
  { -- | Encryption at rest options for the specified domain.
    options :: EncryptionAtRestOptions,
    -- | The status of the encryption at rest options for the specified domain.
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
-- 'options', 'encryptionAtRestOptionsStatus_options' - Encryption at rest options for the specified domain.
--
-- 'status', 'encryptionAtRestOptionsStatus_status' - The status of the encryption at rest options for the specified domain.
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

-- | Encryption at rest options for the specified domain.
encryptionAtRestOptionsStatus_options :: Lens.Lens' EncryptionAtRestOptionsStatus EncryptionAtRestOptions
encryptionAtRestOptionsStatus_options = Lens.lens (\EncryptionAtRestOptionsStatus' {options} -> options) (\s@EncryptionAtRestOptionsStatus' {} a -> s {options = a} :: EncryptionAtRestOptionsStatus)

-- | The status of the encryption at rest options for the specified domain.
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
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData EncryptionAtRestOptionsStatus where
  rnf EncryptionAtRestOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
