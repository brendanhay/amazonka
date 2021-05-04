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
-- Module      : Network.AWS.S3.Types.TargetGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TargetGrant where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.BucketLogsPermission
import Network.AWS.S3.Types.Grantee

-- | Container for granting information.
--
-- /See:/ 'newTargetGrant' smart constructor.
data TargetGrant = TargetGrant'
  { -- | Container for the person being granted permissions.
    grantee :: Prelude.Maybe Grantee,
    -- | Logging permissions assigned to the grantee for the bucket.
    permission :: Prelude.Maybe BucketLogsPermission
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grantee', 'targetGrant_grantee' - Container for the person being granted permissions.
--
-- 'permission', 'targetGrant_permission' - Logging permissions assigned to the grantee for the bucket.
newTargetGrant ::
  TargetGrant
newTargetGrant =
  TargetGrant'
    { grantee = Prelude.Nothing,
      permission = Prelude.Nothing
    }

-- | Container for the person being granted permissions.
targetGrant_grantee :: Lens.Lens' TargetGrant (Prelude.Maybe Grantee)
targetGrant_grantee = Lens.lens (\TargetGrant' {grantee} -> grantee) (\s@TargetGrant' {} a -> s {grantee = a} :: TargetGrant)

-- | Logging permissions assigned to the grantee for the bucket.
targetGrant_permission :: Lens.Lens' TargetGrant (Prelude.Maybe BucketLogsPermission)
targetGrant_permission = Lens.lens (\TargetGrant' {permission} -> permission) (\s@TargetGrant' {} a -> s {permission = a} :: TargetGrant)

instance Prelude.FromXML TargetGrant where
  parseXML x =
    TargetGrant'
      Prelude.<$> (x Prelude..@? "Grantee")
      Prelude.<*> (x Prelude..@? "Permission")

instance Prelude.Hashable TargetGrant

instance Prelude.NFData TargetGrant

instance Prelude.ToXML TargetGrant where
  toXML TargetGrant' {..} =
    Prelude.mconcat
      [ "Grantee" Prelude.@= grantee,
        "Permission" Prelude.@= permission
      ]
