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
-- Module      : Amazonka.S3.Types.SSEKMS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.SSEKMS where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
-- /See:/ 'newSSEKMS' smart constructor.
data SSEKMS = SSEKMS'
  { -- | Specifies the ID of the Amazon Web Services Key Management Service
    -- (Amazon Web Services KMS) symmetric customer managed key to use for
    -- encrypting inventory reports.
    keyId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSEKMS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyId', 'ssekms_keyId' - Specifies the ID of the Amazon Web Services Key Management Service
-- (Amazon Web Services KMS) symmetric customer managed key to use for
-- encrypting inventory reports.
newSSEKMS ::
  -- | 'keyId'
  Prelude.Text ->
  SSEKMS
newSSEKMS pKeyId_ =
  SSEKMS' {keyId = Data._Sensitive Lens.# pKeyId_}

-- | Specifies the ID of the Amazon Web Services Key Management Service
-- (Amazon Web Services KMS) symmetric customer managed key to use for
-- encrypting inventory reports.
ssekms_keyId :: Lens.Lens' SSEKMS Prelude.Text
ssekms_keyId = Lens.lens (\SSEKMS' {keyId} -> keyId) (\s@SSEKMS' {} a -> s {keyId = a} :: SSEKMS) Prelude.. Data._Sensitive

instance Data.FromXML SSEKMS where
  parseXML x = SSEKMS' Prelude.<$> (x Data..@ "KeyId")

instance Prelude.Hashable SSEKMS where
  hashWithSalt _salt SSEKMS' {..} =
    _salt `Prelude.hashWithSalt` keyId

instance Prelude.NFData SSEKMS where
  rnf SSEKMS' {..} = Prelude.rnf keyId

instance Data.ToXML SSEKMS where
  toXML SSEKMS' {..} =
    Prelude.mconcat ["KeyId" Data.@= keyId]
