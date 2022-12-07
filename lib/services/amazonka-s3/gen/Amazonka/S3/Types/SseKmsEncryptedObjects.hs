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
-- Module      : Amazonka.S3.Types.SseKmsEncryptedObjects
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.SseKmsEncryptedObjects where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.SseKmsEncryptedObjectsStatus

-- | A container for filter information for the selection of S3 objects
-- encrypted with Amazon Web Services KMS.
--
-- /See:/ 'newSseKmsEncryptedObjects' smart constructor.
data SseKmsEncryptedObjects = SseKmsEncryptedObjects'
  { -- | Specifies whether Amazon S3 replicates objects created with server-side
    -- encryption using an Amazon Web Services KMS key stored in Amazon Web
    -- Services Key Management Service.
    status :: SseKmsEncryptedObjectsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SseKmsEncryptedObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'sseKmsEncryptedObjects_status' - Specifies whether Amazon S3 replicates objects created with server-side
-- encryption using an Amazon Web Services KMS key stored in Amazon Web
-- Services Key Management Service.
newSseKmsEncryptedObjects ::
  -- | 'status'
  SseKmsEncryptedObjectsStatus ->
  SseKmsEncryptedObjects
newSseKmsEncryptedObjects pStatus_ =
  SseKmsEncryptedObjects' {status = pStatus_}

-- | Specifies whether Amazon S3 replicates objects created with server-side
-- encryption using an Amazon Web Services KMS key stored in Amazon Web
-- Services Key Management Service.
sseKmsEncryptedObjects_status :: Lens.Lens' SseKmsEncryptedObjects SseKmsEncryptedObjectsStatus
sseKmsEncryptedObjects_status = Lens.lens (\SseKmsEncryptedObjects' {status} -> status) (\s@SseKmsEncryptedObjects' {} a -> s {status = a} :: SseKmsEncryptedObjects)

instance Data.FromXML SseKmsEncryptedObjects where
  parseXML x =
    SseKmsEncryptedObjects'
      Prelude.<$> (x Data..@ "Status")

instance Prelude.Hashable SseKmsEncryptedObjects where
  hashWithSalt _salt SseKmsEncryptedObjects' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData SseKmsEncryptedObjects where
  rnf SseKmsEncryptedObjects' {..} = Prelude.rnf status

instance Data.ToXML SseKmsEncryptedObjects where
  toXML SseKmsEncryptedObjects' {..} =
    Prelude.mconcat ["Status" Data.@= status]
