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
-- Module      : Amazonka.S3.Types.VersioningConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.VersioningConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.BucketVersioningStatus
import Amazonka.S3.Types.MFADelete

-- | Describes the versioning state of an Amazon S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTVersioningStatus.html PUT Bucket versioning>
-- in the /Amazon S3 API Reference/.
--
-- /See:/ 'newVersioningConfiguration' smart constructor.
data VersioningConfiguration = VersioningConfiguration'
  { -- | The versioning state of the bucket.
    status :: Prelude.Maybe BucketVersioningStatus,
    -- | Specifies whether MFA delete is enabled in the bucket versioning
    -- configuration. This element is only returned if the bucket has been
    -- configured with MFA delete. If the bucket has never been so configured,
    -- this element is not returned.
    mfaDelete :: Prelude.Maybe MFADelete
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersioningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'versioningConfiguration_status' - The versioning state of the bucket.
--
-- 'mfaDelete', 'versioningConfiguration_mfaDelete' - Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
newVersioningConfiguration ::
  VersioningConfiguration
newVersioningConfiguration =
  VersioningConfiguration'
    { status = Prelude.Nothing,
      mfaDelete = Prelude.Nothing
    }

-- | The versioning state of the bucket.
versioningConfiguration_status :: Lens.Lens' VersioningConfiguration (Prelude.Maybe BucketVersioningStatus)
versioningConfiguration_status = Lens.lens (\VersioningConfiguration' {status} -> status) (\s@VersioningConfiguration' {} a -> s {status = a} :: VersioningConfiguration)

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
versioningConfiguration_mfaDelete :: Lens.Lens' VersioningConfiguration (Prelude.Maybe MFADelete)
versioningConfiguration_mfaDelete = Lens.lens (\VersioningConfiguration' {mfaDelete} -> mfaDelete) (\s@VersioningConfiguration' {} a -> s {mfaDelete = a} :: VersioningConfiguration)

instance Prelude.Hashable VersioningConfiguration where
  hashWithSalt _salt VersioningConfiguration' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` mfaDelete

instance Prelude.NFData VersioningConfiguration where
  rnf VersioningConfiguration' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf mfaDelete

instance Core.ToXML VersioningConfiguration where
  toXML VersioningConfiguration' {..} =
    Prelude.mconcat
      [ "Status" Core.@= status,
        "MfaDelete" Core.@= mfaDelete
      ]
