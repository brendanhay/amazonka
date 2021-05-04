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
-- Module      : Network.AWS.S3.Types.AccessControlPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccessControlPolicy where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Grant
import Network.AWS.S3.Types.Owner

-- | Contains the elements that set the ACL permissions for an object per
-- grantee.
--
-- /See:/ 'newAccessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
  { -- | Container for the bucket owner\'s display name and ID.
    owner :: Prelude.Maybe Owner,
    -- | A list of grants.
    grants :: Prelude.Maybe [Grant]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { owner = Prelude.Nothing,
      grants = Prelude.Nothing
    }

-- | Container for the bucket owner\'s display name and ID.
accessControlPolicy_owner :: Lens.Lens' AccessControlPolicy (Prelude.Maybe Owner)
accessControlPolicy_owner = Lens.lens (\AccessControlPolicy' {owner} -> owner) (\s@AccessControlPolicy' {} a -> s {owner = a} :: AccessControlPolicy)

-- | A list of grants.
accessControlPolicy_grants :: Lens.Lens' AccessControlPolicy (Prelude.Maybe [Grant])
accessControlPolicy_grants = Lens.lens (\AccessControlPolicy' {grants} -> grants) (\s@AccessControlPolicy' {} a -> s {grants = a} :: AccessControlPolicy) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable AccessControlPolicy

instance Prelude.NFData AccessControlPolicy

instance Prelude.ToXML AccessControlPolicy where
  toXML AccessControlPolicy' {..} =
    Prelude.mconcat
      [ "Owner" Prelude.@= owner,
        "AccessControlList"
          Prelude.@= Prelude.toXML
            (Prelude.toXMLList "Grant" Prelude.<$> grants)
      ]
