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
-- Module      : Network.AWS.IAM.Types.RoleLastUsed
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.RoleLastUsed where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the last time that an IAM role was used. This
-- includes the date and time and the Region in which the role was last
-- used. Activity is only reported for the trailing 400 days. This period
-- can be shorter if your Region began supporting these features within the
-- last year. The role might have been used more than 400 days ago. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
--
-- This data type is returned as a response element in the GetRole and
-- GetAccountAuthorizationDetails operations.
--
-- /See:/ 'newRoleLastUsed' smart constructor.
data RoleLastUsed = RoleLastUsed'
  { -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> that the
    -- role was last used.
    --
    -- This field is null if the role has not been used within the IAM tracking
    -- period. For more information about the tracking period, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
    -- in the /IAM User Guide/.
    lastUsedDate :: Prelude.Maybe Prelude.ISO8601,
    -- | The name of the AWS Region in which the role was last used.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RoleLastUsed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUsedDate', 'roleLastUsed_lastUsedDate' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> that the
-- role was last used.
--
-- This field is null if the role has not been used within the IAM tracking
-- period. For more information about the tracking period, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
--
-- 'region', 'roleLastUsed_region' - The name of the AWS Region in which the role was last used.
newRoleLastUsed ::
  RoleLastUsed
newRoleLastUsed =
  RoleLastUsed'
    { lastUsedDate = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> that the
-- role was last used.
--
-- This field is null if the role has not been used within the IAM tracking
-- period. For more information about the tracking period, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions where data is tracked>
-- in the /IAM User Guide/.
roleLastUsed_lastUsedDate :: Lens.Lens' RoleLastUsed (Prelude.Maybe Prelude.UTCTime)
roleLastUsed_lastUsedDate = Lens.lens (\RoleLastUsed' {lastUsedDate} -> lastUsedDate) (\s@RoleLastUsed' {} a -> s {lastUsedDate = a} :: RoleLastUsed) Prelude.. Lens.mapping Prelude._Time

-- | The name of the AWS Region in which the role was last used.
roleLastUsed_region :: Lens.Lens' RoleLastUsed (Prelude.Maybe Prelude.Text)
roleLastUsed_region = Lens.lens (\RoleLastUsed' {region} -> region) (\s@RoleLastUsed' {} a -> s {region = a} :: RoleLastUsed)

instance Prelude.FromXML RoleLastUsed where
  parseXML x =
    RoleLastUsed'
      Prelude.<$> (x Prelude..@? "LastUsedDate")
      Prelude.<*> (x Prelude..@? "Region")

instance Prelude.Hashable RoleLastUsed

instance Prelude.NFData RoleLastUsed
