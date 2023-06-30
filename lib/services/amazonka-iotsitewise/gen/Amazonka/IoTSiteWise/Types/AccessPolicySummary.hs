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
-- Module      : Amazonka.IoTSiteWise.Types.AccessPolicySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AccessPolicySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.Identity
import Amazonka.IoTSiteWise.Types.Permission
import Amazonka.IoTSiteWise.Types.Resource
import qualified Amazonka.Prelude as Prelude

-- | Contains an access policy that defines an identity\'s access to an IoT
-- SiteWise Monitor resource.
--
-- /See:/ 'newAccessPolicySummary' smart constructor.
data AccessPolicySummary = AccessPolicySummary'
  { -- | The date the access policy was created, in Unix epoch time.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The date the access policy was last updated, in Unix epoch time.
    lastUpdateDate :: Prelude.Maybe Data.POSIX,
    -- | The ID of the access policy.
    id :: Prelude.Text,
    -- | The identity (an IAM Identity Center user, an IAM Identity Center group,
    -- or an IAM user).
    identity :: Identity,
    -- | The IoT SiteWise Monitor resource (a portal or project).
    resource :: Resource,
    -- | The permissions for the access policy. Note that a project
    -- @ADMINISTRATOR@ is also known as a project owner.
    permission :: Permission
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessPolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'accessPolicySummary_creationDate' - The date the access policy was created, in Unix epoch time.
--
-- 'lastUpdateDate', 'accessPolicySummary_lastUpdateDate' - The date the access policy was last updated, in Unix epoch time.
--
-- 'id', 'accessPolicySummary_id' - The ID of the access policy.
--
-- 'identity', 'accessPolicySummary_identity' - The identity (an IAM Identity Center user, an IAM Identity Center group,
-- or an IAM user).
--
-- 'resource', 'accessPolicySummary_resource' - The IoT SiteWise Monitor resource (a portal or project).
--
-- 'permission', 'accessPolicySummary_permission' - The permissions for the access policy. Note that a project
-- @ADMINISTRATOR@ is also known as a project owner.
newAccessPolicySummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'identity'
  Identity ->
  -- | 'resource'
  Resource ->
  -- | 'permission'
  Permission ->
  AccessPolicySummary
newAccessPolicySummary
  pId_
  pIdentity_
  pResource_
  pPermission_ =
    AccessPolicySummary'
      { creationDate =
          Prelude.Nothing,
        lastUpdateDate = Prelude.Nothing,
        id = pId_,
        identity = pIdentity_,
        resource = pResource_,
        permission = pPermission_
      }

-- | The date the access policy was created, in Unix epoch time.
accessPolicySummary_creationDate :: Lens.Lens' AccessPolicySummary (Prelude.Maybe Prelude.UTCTime)
accessPolicySummary_creationDate = Lens.lens (\AccessPolicySummary' {creationDate} -> creationDate) (\s@AccessPolicySummary' {} a -> s {creationDate = a} :: AccessPolicySummary) Prelude.. Lens.mapping Data._Time

-- | The date the access policy was last updated, in Unix epoch time.
accessPolicySummary_lastUpdateDate :: Lens.Lens' AccessPolicySummary (Prelude.Maybe Prelude.UTCTime)
accessPolicySummary_lastUpdateDate = Lens.lens (\AccessPolicySummary' {lastUpdateDate} -> lastUpdateDate) (\s@AccessPolicySummary' {} a -> s {lastUpdateDate = a} :: AccessPolicySummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the access policy.
accessPolicySummary_id :: Lens.Lens' AccessPolicySummary Prelude.Text
accessPolicySummary_id = Lens.lens (\AccessPolicySummary' {id} -> id) (\s@AccessPolicySummary' {} a -> s {id = a} :: AccessPolicySummary)

-- | The identity (an IAM Identity Center user, an IAM Identity Center group,
-- or an IAM user).
accessPolicySummary_identity :: Lens.Lens' AccessPolicySummary Identity
accessPolicySummary_identity = Lens.lens (\AccessPolicySummary' {identity} -> identity) (\s@AccessPolicySummary' {} a -> s {identity = a} :: AccessPolicySummary)

-- | The IoT SiteWise Monitor resource (a portal or project).
accessPolicySummary_resource :: Lens.Lens' AccessPolicySummary Resource
accessPolicySummary_resource = Lens.lens (\AccessPolicySummary' {resource} -> resource) (\s@AccessPolicySummary' {} a -> s {resource = a} :: AccessPolicySummary)

-- | The permissions for the access policy. Note that a project
-- @ADMINISTRATOR@ is also known as a project owner.
accessPolicySummary_permission :: Lens.Lens' AccessPolicySummary Permission
accessPolicySummary_permission = Lens.lens (\AccessPolicySummary' {permission} -> permission) (\s@AccessPolicySummary' {} a -> s {permission = a} :: AccessPolicySummary)

instance Data.FromJSON AccessPolicySummary where
  parseJSON =
    Data.withObject
      "AccessPolicySummary"
      ( \x ->
          AccessPolicySummary'
            Prelude.<$> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "lastUpdateDate")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "identity")
            Prelude.<*> (x Data..: "resource")
            Prelude.<*> (x Data..: "permission")
      )

instance Prelude.Hashable AccessPolicySummary where
  hashWithSalt _salt AccessPolicySummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastUpdateDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` permission

instance Prelude.NFData AccessPolicySummary where
  rnf AccessPolicySummary' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf identity
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf permission
