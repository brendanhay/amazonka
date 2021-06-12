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
-- Module      : Network.AWS.S3.Types.AccessControlPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccessControlPolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.Owner

-- | Contains the elements that set the ACL permissions for an object per
-- grantee.
--
-- /See:/ 'newAccessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
  { -- | Container for the bucket owner\'s display name and ID.
    owner :: Core.Maybe Owner,
    -- | A list of grants.
    grants :: Core.Maybe [Grant]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccessControlPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'owner', 'accessControlPolicy_owner' - Container for the bucket owner\'s display name and ID.
--
-- 'grants', 'accessControlPolicy_grants' - A list of grants.
newAccessControlPolicy ::
  AccessControlPolicy
newAccessControlPolicy =
  AccessControlPolicy'
    { owner = Core.Nothing,
      grants = Core.Nothing
    }

-- | Container for the bucket owner\'s display name and ID.
accessControlPolicy_owner :: Lens.Lens' AccessControlPolicy (Core.Maybe Owner)
accessControlPolicy_owner = Lens.lens (\AccessControlPolicy' {owner} -> owner) (\s@AccessControlPolicy' {} a -> s {owner = a} :: AccessControlPolicy)

-- | A list of grants.
accessControlPolicy_grants :: Lens.Lens' AccessControlPolicy (Core.Maybe [Grant])
accessControlPolicy_grants = Lens.lens (\AccessControlPolicy' {grants} -> grants) (\s@AccessControlPolicy' {} a -> s {grants = a} :: AccessControlPolicy) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable AccessControlPolicy

instance Core.NFData AccessControlPolicy

instance Core.ToXML AccessControlPolicy where
  toXML AccessControlPolicy' {..} =
    Core.mconcat
      [ "Owner" Core.@= owner,
        "AccessControlList"
          Core.@= Core.toXML (Core.toXMLList "Grant" Core.<$> grants)
      ]
