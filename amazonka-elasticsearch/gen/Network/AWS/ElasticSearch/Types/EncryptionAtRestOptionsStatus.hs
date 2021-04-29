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
-- Module      : Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus where

import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromJSON
    EncryptionAtRestOptionsStatus
  where
  parseJSON =
    Prelude.withObject
      "EncryptionAtRestOptionsStatus"
      ( \x ->
          EncryptionAtRestOptionsStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance
  Prelude.Hashable
    EncryptionAtRestOptionsStatus

instance Prelude.NFData EncryptionAtRestOptionsStatus
