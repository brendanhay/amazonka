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
-- Module      : Amazonka.IAM.Types.TrackedActionLastAccessed
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.TrackedActionLastAccessed where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the most recent attempt to access an action
-- within the service.
--
-- This data type is used as a response element in the
-- GetServiceLastAccessedDetails operation.
--
-- /See:/ 'newTrackedActionLastAccessed' smart constructor.
data TrackedActionLastAccessed = TrackedActionLastAccessed'
  { -- | The name of the tracked action to which access was attempted. Tracked
    -- actions are actions that report activity to IAM.
    actionName :: Prelude.Maybe Prelude.Text,
    lastAccessedEntity :: Prelude.Maybe Prelude.Text,
    -- | The Region from which the authenticated entity (user or role) last
    -- attempted to access the tracked action. Amazon Web Services does not
    -- report unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    lastAccessedRegion :: Prelude.Maybe Prelude.Text,
    -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
    -- authenticated entity most recently attempted to access the tracked
    -- service. Amazon Web Services does not report unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    lastAccessedTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrackedActionLastAccessed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'trackedActionLastAccessed_actionName' - The name of the tracked action to which access was attempted. Tracked
-- actions are actions that report activity to IAM.
--
-- 'lastAccessedEntity', 'trackedActionLastAccessed_lastAccessedEntity' - Undocumented member.
--
-- 'lastAccessedRegion', 'trackedActionLastAccessed_lastAccessedRegion' - The Region from which the authenticated entity (user or role) last
-- attempted to access the tracked action. Amazon Web Services does not
-- report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'lastAccessedTime', 'trackedActionLastAccessed_lastAccessedTime' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated entity most recently attempted to access the tracked
-- service. Amazon Web Services does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
newTrackedActionLastAccessed ::
  TrackedActionLastAccessed
newTrackedActionLastAccessed =
  TrackedActionLastAccessed'
    { actionName =
        Prelude.Nothing,
      lastAccessedEntity = Prelude.Nothing,
      lastAccessedRegion = Prelude.Nothing,
      lastAccessedTime = Prelude.Nothing
    }

-- | The name of the tracked action to which access was attempted. Tracked
-- actions are actions that report activity to IAM.
trackedActionLastAccessed_actionName :: Lens.Lens' TrackedActionLastAccessed (Prelude.Maybe Prelude.Text)
trackedActionLastAccessed_actionName = Lens.lens (\TrackedActionLastAccessed' {actionName} -> actionName) (\s@TrackedActionLastAccessed' {} a -> s {actionName = a} :: TrackedActionLastAccessed)

-- | Undocumented member.
trackedActionLastAccessed_lastAccessedEntity :: Lens.Lens' TrackedActionLastAccessed (Prelude.Maybe Prelude.Text)
trackedActionLastAccessed_lastAccessedEntity = Lens.lens (\TrackedActionLastAccessed' {lastAccessedEntity} -> lastAccessedEntity) (\s@TrackedActionLastAccessed' {} a -> s {lastAccessedEntity = a} :: TrackedActionLastAccessed)

-- | The Region from which the authenticated entity (user or role) last
-- attempted to access the tracked action. Amazon Web Services does not
-- report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
trackedActionLastAccessed_lastAccessedRegion :: Lens.Lens' TrackedActionLastAccessed (Prelude.Maybe Prelude.Text)
trackedActionLastAccessed_lastAccessedRegion = Lens.lens (\TrackedActionLastAccessed' {lastAccessedRegion} -> lastAccessedRegion) (\s@TrackedActionLastAccessed' {} a -> s {lastAccessedRegion = a} :: TrackedActionLastAccessed)

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated entity most recently attempted to access the tracked
-- service. Amazon Web Services does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
trackedActionLastAccessed_lastAccessedTime :: Lens.Lens' TrackedActionLastAccessed (Prelude.Maybe Prelude.UTCTime)
trackedActionLastAccessed_lastAccessedTime = Lens.lens (\TrackedActionLastAccessed' {lastAccessedTime} -> lastAccessedTime) (\s@TrackedActionLastAccessed' {} a -> s {lastAccessedTime = a} :: TrackedActionLastAccessed) Prelude.. Lens.mapping Data._Time

instance Data.FromXML TrackedActionLastAccessed where
  parseXML x =
    TrackedActionLastAccessed'
      Prelude.<$> (x Data..@? "ActionName")
      Prelude.<*> (x Data..@? "LastAccessedEntity")
      Prelude.<*> (x Data..@? "LastAccessedRegion")
      Prelude.<*> (x Data..@? "LastAccessedTime")

instance Prelude.Hashable TrackedActionLastAccessed where
  hashWithSalt _salt TrackedActionLastAccessed' {..} =
    _salt `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` lastAccessedEntity
      `Prelude.hashWithSalt` lastAccessedRegion
      `Prelude.hashWithSalt` lastAccessedTime

instance Prelude.NFData TrackedActionLastAccessed where
  rnf TrackedActionLastAccessed' {..} =
    Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf lastAccessedEntity
      `Prelude.seq` Prelude.rnf lastAccessedRegion
      `Prelude.seq` Prelude.rnf lastAccessedTime
