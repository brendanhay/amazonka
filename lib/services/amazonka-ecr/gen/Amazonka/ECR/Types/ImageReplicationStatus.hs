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
-- Module      : Amazonka.ECR.Types.ImageReplicationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageReplicationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.ReplicationStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of the replication process for an image.
--
-- /See:/ 'newImageReplicationStatus' smart constructor.
data ImageReplicationStatus = ImageReplicationStatus'
  { -- | The failure code for a replication that has failed.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The destination Region for the image replication.
    region :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID associated with the registry to which
    -- the image belongs.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The image replication status.
    status :: Prelude.Maybe ReplicationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageReplicationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCode', 'imageReplicationStatus_failureCode' - The failure code for a replication that has failed.
--
-- 'region', 'imageReplicationStatus_region' - The destination Region for the image replication.
--
-- 'registryId', 'imageReplicationStatus_registryId' - The Amazon Web Services account ID associated with the registry to which
-- the image belongs.
--
-- 'status', 'imageReplicationStatus_status' - The image replication status.
newImageReplicationStatus ::
  ImageReplicationStatus
newImageReplicationStatus =
  ImageReplicationStatus'
    { failureCode =
        Prelude.Nothing,
      region = Prelude.Nothing,
      registryId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The failure code for a replication that has failed.
imageReplicationStatus_failureCode :: Lens.Lens' ImageReplicationStatus (Prelude.Maybe Prelude.Text)
imageReplicationStatus_failureCode = Lens.lens (\ImageReplicationStatus' {failureCode} -> failureCode) (\s@ImageReplicationStatus' {} a -> s {failureCode = a} :: ImageReplicationStatus)

-- | The destination Region for the image replication.
imageReplicationStatus_region :: Lens.Lens' ImageReplicationStatus (Prelude.Maybe Prelude.Text)
imageReplicationStatus_region = Lens.lens (\ImageReplicationStatus' {region} -> region) (\s@ImageReplicationStatus' {} a -> s {region = a} :: ImageReplicationStatus)

-- | The Amazon Web Services account ID associated with the registry to which
-- the image belongs.
imageReplicationStatus_registryId :: Lens.Lens' ImageReplicationStatus (Prelude.Maybe Prelude.Text)
imageReplicationStatus_registryId = Lens.lens (\ImageReplicationStatus' {registryId} -> registryId) (\s@ImageReplicationStatus' {} a -> s {registryId = a} :: ImageReplicationStatus)

-- | The image replication status.
imageReplicationStatus_status :: Lens.Lens' ImageReplicationStatus (Prelude.Maybe ReplicationStatus)
imageReplicationStatus_status = Lens.lens (\ImageReplicationStatus' {status} -> status) (\s@ImageReplicationStatus' {} a -> s {status = a} :: ImageReplicationStatus)

instance Data.FromJSON ImageReplicationStatus where
  parseJSON =
    Data.withObject
      "ImageReplicationStatus"
      ( \x ->
          ImageReplicationStatus'
            Prelude.<$> (x Data..:? "failureCode")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "registryId")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable ImageReplicationStatus where
  hashWithSalt _salt ImageReplicationStatus' {..} =
    _salt
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImageReplicationStatus where
  rnf ImageReplicationStatus' {..} =
    Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf status
