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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageReplicationStatus where

import qualified Amazonka.Core as Core
import Amazonka.ECR.Types.ReplicationStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The status of the replication process for an image.
--
-- /See:/ 'newImageReplicationStatus' smart constructor.
data ImageReplicationStatus = ImageReplicationStatus'
  { -- | The image replication status.
    status :: Prelude.Maybe ReplicationStatus,
    -- | The failure code for a replication that has failed.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID associated with the registry to which the image
    -- belongs.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The destination Region for the image replication.
    region :: Prelude.Maybe Prelude.Text
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
-- 'status', 'imageReplicationStatus_status' - The image replication status.
--
-- 'failureCode', 'imageReplicationStatus_failureCode' - The failure code for a replication that has failed.
--
-- 'registryId', 'imageReplicationStatus_registryId' - The AWS account ID associated with the registry to which the image
-- belongs.
--
-- 'region', 'imageReplicationStatus_region' - The destination Region for the image replication.
newImageReplicationStatus ::
  ImageReplicationStatus
newImageReplicationStatus =
  ImageReplicationStatus'
    { status = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      registryId = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The image replication status.
imageReplicationStatus_status :: Lens.Lens' ImageReplicationStatus (Prelude.Maybe ReplicationStatus)
imageReplicationStatus_status = Lens.lens (\ImageReplicationStatus' {status} -> status) (\s@ImageReplicationStatus' {} a -> s {status = a} :: ImageReplicationStatus)

-- | The failure code for a replication that has failed.
imageReplicationStatus_failureCode :: Lens.Lens' ImageReplicationStatus (Prelude.Maybe Prelude.Text)
imageReplicationStatus_failureCode = Lens.lens (\ImageReplicationStatus' {failureCode} -> failureCode) (\s@ImageReplicationStatus' {} a -> s {failureCode = a} :: ImageReplicationStatus)

-- | The AWS account ID associated with the registry to which the image
-- belongs.
imageReplicationStatus_registryId :: Lens.Lens' ImageReplicationStatus (Prelude.Maybe Prelude.Text)
imageReplicationStatus_registryId = Lens.lens (\ImageReplicationStatus' {registryId} -> registryId) (\s@ImageReplicationStatus' {} a -> s {registryId = a} :: ImageReplicationStatus)

-- | The destination Region for the image replication.
imageReplicationStatus_region :: Lens.Lens' ImageReplicationStatus (Prelude.Maybe Prelude.Text)
imageReplicationStatus_region = Lens.lens (\ImageReplicationStatus' {region} -> region) (\s@ImageReplicationStatus' {} a -> s {region = a} :: ImageReplicationStatus)

instance Core.FromJSON ImageReplicationStatus where
  parseJSON =
    Core.withObject
      "ImageReplicationStatus"
      ( \x ->
          ImageReplicationStatus'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "failureCode")
            Prelude.<*> (x Core..:? "registryId")
            Prelude.<*> (x Core..:? "region")
      )

instance Prelude.Hashable ImageReplicationStatus

instance Prelude.NFData ImageReplicationStatus
