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
-- Module      : Amazonka.S3.Types.SourceSelectionCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.SourceSelectionCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ReplicaModifications
import Amazonka.S3.Types.SseKmsEncryptedObjects

-- | A container that describes additional filters for identifying the source
-- objects that you want to replicate. You can choose to enable or disable
-- the replication of these objects. Currently, Amazon S3 supports only the
-- filter that you can specify for objects created with server-side
-- encryption using a customer managed key stored in Amazon Web Services
-- Key Management Service (SSE-KMS).
--
-- /See:/ 'newSourceSelectionCriteria' smart constructor.
data SourceSelectionCriteria = SourceSelectionCriteria'
  { -- | A filter that you can specify for selections for modifications on
    -- replicas. Amazon S3 doesn\'t replicate replica modifications by default.
    -- In the latest version of replication configuration (when @Filter@ is
    -- specified), you can specify this element and set the status to @Enabled@
    -- to replicate modifications on replicas.
    --
    -- If you don\'t specify the @Filter@ element, Amazon S3 assumes that the
    -- replication configuration is the earlier version, V1. In the earlier
    -- version, this element is not allowed
    replicaModifications :: Prelude.Maybe ReplicaModifications,
    -- | A container for filter information for the selection of Amazon S3
    -- objects encrypted with Amazon Web Services KMS. If you include
    -- @SourceSelectionCriteria@ in the replication configuration, this element
    -- is required.
    sseKmsEncryptedObjects :: Prelude.Maybe SseKmsEncryptedObjects
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceSelectionCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicaModifications', 'sourceSelectionCriteria_replicaModifications' - A filter that you can specify for selections for modifications on
-- replicas. Amazon S3 doesn\'t replicate replica modifications by default.
-- In the latest version of replication configuration (when @Filter@ is
-- specified), you can specify this element and set the status to @Enabled@
-- to replicate modifications on replicas.
--
-- If you don\'t specify the @Filter@ element, Amazon S3 assumes that the
-- replication configuration is the earlier version, V1. In the earlier
-- version, this element is not allowed
--
-- 'sseKmsEncryptedObjects', 'sourceSelectionCriteria_sseKmsEncryptedObjects' - A container for filter information for the selection of Amazon S3
-- objects encrypted with Amazon Web Services KMS. If you include
-- @SourceSelectionCriteria@ in the replication configuration, this element
-- is required.
newSourceSelectionCriteria ::
  SourceSelectionCriteria
newSourceSelectionCriteria =
  SourceSelectionCriteria'
    { replicaModifications =
        Prelude.Nothing,
      sseKmsEncryptedObjects = Prelude.Nothing
    }

-- | A filter that you can specify for selections for modifications on
-- replicas. Amazon S3 doesn\'t replicate replica modifications by default.
-- In the latest version of replication configuration (when @Filter@ is
-- specified), you can specify this element and set the status to @Enabled@
-- to replicate modifications on replicas.
--
-- If you don\'t specify the @Filter@ element, Amazon S3 assumes that the
-- replication configuration is the earlier version, V1. In the earlier
-- version, this element is not allowed
sourceSelectionCriteria_replicaModifications :: Lens.Lens' SourceSelectionCriteria (Prelude.Maybe ReplicaModifications)
sourceSelectionCriteria_replicaModifications = Lens.lens (\SourceSelectionCriteria' {replicaModifications} -> replicaModifications) (\s@SourceSelectionCriteria' {} a -> s {replicaModifications = a} :: SourceSelectionCriteria)

-- | A container for filter information for the selection of Amazon S3
-- objects encrypted with Amazon Web Services KMS. If you include
-- @SourceSelectionCriteria@ in the replication configuration, this element
-- is required.
sourceSelectionCriteria_sseKmsEncryptedObjects :: Lens.Lens' SourceSelectionCriteria (Prelude.Maybe SseKmsEncryptedObjects)
sourceSelectionCriteria_sseKmsEncryptedObjects = Lens.lens (\SourceSelectionCriteria' {sseKmsEncryptedObjects} -> sseKmsEncryptedObjects) (\s@SourceSelectionCriteria' {} a -> s {sseKmsEncryptedObjects = a} :: SourceSelectionCriteria)

instance Data.FromXML SourceSelectionCriteria where
  parseXML x =
    SourceSelectionCriteria'
      Prelude.<$> (x Data..@? "ReplicaModifications")
      Prelude.<*> (x Data..@? "SseKmsEncryptedObjects")

instance Prelude.Hashable SourceSelectionCriteria where
  hashWithSalt _salt SourceSelectionCriteria' {..} =
    _salt `Prelude.hashWithSalt` replicaModifications
      `Prelude.hashWithSalt` sseKmsEncryptedObjects

instance Prelude.NFData SourceSelectionCriteria where
  rnf SourceSelectionCriteria' {..} =
    Prelude.rnf replicaModifications
      `Prelude.seq` Prelude.rnf sseKmsEncryptedObjects

instance Data.ToXML SourceSelectionCriteria where
  toXML SourceSelectionCriteria' {..} =
    Prelude.mconcat
      [ "ReplicaModifications" Data.@= replicaModifications,
        "SseKmsEncryptedObjects"
          Data.@= sseKmsEncryptedObjects
      ]
