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
-- Module      : Amazonka.S3.Types.AccessControlPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.AccessControlPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Grant
import Amazonka.S3.Types.Owner

-- | Contains the elements that set the ACL permissions for an object per
-- grantee.
--
-- /See:/ 'newAccessControlPolicy' smart constructor.
data AccessControlPolicy = AccessControlPolicy'
  { -- | A list of grants.
    grants :: Prelude.Maybe [Grant],
    -- | Container for the bucket owner\'s display name and ID.
    owner :: Prelude.Maybe Owner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessControlPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'grants', 'accessControlPolicy_grants' - A list of grants.
--
-- 'owner', 'accessControlPolicy_owner' - Container for the bucket owner\'s display name and ID.
newAccessControlPolicy ::
  AccessControlPolicy
newAccessControlPolicy =
  AccessControlPolicy'
    { grants = Prelude.Nothing,
      owner = Prelude.Nothing
    }

-- | A list of grants.
accessControlPolicy_grants :: Lens.Lens' AccessControlPolicy (Prelude.Maybe [Grant])
accessControlPolicy_grants = Lens.lens (\AccessControlPolicy' {grants} -> grants) (\s@AccessControlPolicy' {} a -> s {grants = a} :: AccessControlPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Container for the bucket owner\'s display name and ID.
accessControlPolicy_owner :: Lens.Lens' AccessControlPolicy (Prelude.Maybe Owner)
accessControlPolicy_owner = Lens.lens (\AccessControlPolicy' {owner} -> owner) (\s@AccessControlPolicy' {} a -> s {owner = a} :: AccessControlPolicy)

instance Prelude.Hashable AccessControlPolicy where
  hashWithSalt _salt AccessControlPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` grants
      `Prelude.hashWithSalt` owner

instance Prelude.NFData AccessControlPolicy where
  rnf AccessControlPolicy' {..} =
    Prelude.rnf grants `Prelude.seq` Prelude.rnf owner

instance Data.ToXML AccessControlPolicy where
  toXML AccessControlPolicy' {..} =
    Prelude.mconcat
      [ "AccessControlList"
          Data.@= Data.toXML
            (Data.toXMLList "Grant" Prelude.<$> grants),
        "Owner" Data.@= owner
      ]
