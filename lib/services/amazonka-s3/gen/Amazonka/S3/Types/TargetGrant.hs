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
-- Module      : Amazonka.S3.Types.TargetGrant
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.TargetGrant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.BucketLogsPermission
import Amazonka.S3.Types.Grantee

-- | Container for granting information.
--
-- Buckets that use the bucket owner enforced setting for Object Ownership
-- don\'t support target grants. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/enable-server-access-logging.html#grant-log-delivery-permissions-general Permissions server access log delivery>
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newTargetGrant' smart constructor.
data TargetGrant = TargetGrant'
  { -- | Logging permissions assigned to the grantee for the bucket.
    permission :: Prelude.Maybe BucketLogsPermission,
    -- | Container for the person being granted permissions.
    grantee :: Prelude.Maybe Grantee
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permission', 'targetGrant_permission' - Logging permissions assigned to the grantee for the bucket.
--
-- 'grantee', 'targetGrant_grantee' - Container for the person being granted permissions.
newTargetGrant ::
  TargetGrant
newTargetGrant =
  TargetGrant'
    { permission = Prelude.Nothing,
      grantee = Prelude.Nothing
    }

-- | Logging permissions assigned to the grantee for the bucket.
targetGrant_permission :: Lens.Lens' TargetGrant (Prelude.Maybe BucketLogsPermission)
targetGrant_permission = Lens.lens (\TargetGrant' {permission} -> permission) (\s@TargetGrant' {} a -> s {permission = a} :: TargetGrant)

-- | Container for the person being granted permissions.
targetGrant_grantee :: Lens.Lens' TargetGrant (Prelude.Maybe Grantee)
targetGrant_grantee = Lens.lens (\TargetGrant' {grantee} -> grantee) (\s@TargetGrant' {} a -> s {grantee = a} :: TargetGrant)

instance Core.FromXML TargetGrant where
  parseXML x =
    TargetGrant'
      Prelude.<$> (x Core..@? "Permission")
      Prelude.<*> (x Core..@? "Grantee")

instance Prelude.Hashable TargetGrant where
  hashWithSalt _salt TargetGrant' {..} =
    _salt `Prelude.hashWithSalt` permission
      `Prelude.hashWithSalt` grantee

instance Prelude.NFData TargetGrant where
  rnf TargetGrant' {..} =
    Prelude.rnf permission
      `Prelude.seq` Prelude.rnf grantee

instance Core.ToXML TargetGrant where
  toXML TargetGrant' {..} =
    Prelude.mconcat
      [ "Permission" Core.@= permission,
        "Grantee" Core.@= grantee
      ]
