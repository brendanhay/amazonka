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
-- Module      : Network.AWS.GuardDuty.Types.AccessControlList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccessControlList where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information on the current access control policies for the
-- bucket.
--
-- /See:/ 'newAccessControlList' smart constructor.
data AccessControlList = AccessControlList'
  { -- | A value that indicates whether public read access for the bucket is
    -- enabled through an Access Control List (ACL).
    allowsPublicReadAccess :: Core.Maybe Core.Bool,
    -- | A value that indicates whether public write access for the bucket is
    -- enabled through an Access Control List (ACL).
    allowsPublicWriteAccess :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccessControlList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowsPublicReadAccess', 'accessControlList_allowsPublicReadAccess' - A value that indicates whether public read access for the bucket is
-- enabled through an Access Control List (ACL).
--
-- 'allowsPublicWriteAccess', 'accessControlList_allowsPublicWriteAccess' - A value that indicates whether public write access for the bucket is
-- enabled through an Access Control List (ACL).
newAccessControlList ::
  AccessControlList
newAccessControlList =
  AccessControlList'
    { allowsPublicReadAccess =
        Core.Nothing,
      allowsPublicWriteAccess = Core.Nothing
    }

-- | A value that indicates whether public read access for the bucket is
-- enabled through an Access Control List (ACL).
accessControlList_allowsPublicReadAccess :: Lens.Lens' AccessControlList (Core.Maybe Core.Bool)
accessControlList_allowsPublicReadAccess = Lens.lens (\AccessControlList' {allowsPublicReadAccess} -> allowsPublicReadAccess) (\s@AccessControlList' {} a -> s {allowsPublicReadAccess = a} :: AccessControlList)

-- | A value that indicates whether public write access for the bucket is
-- enabled through an Access Control List (ACL).
accessControlList_allowsPublicWriteAccess :: Lens.Lens' AccessControlList (Core.Maybe Core.Bool)
accessControlList_allowsPublicWriteAccess = Lens.lens (\AccessControlList' {allowsPublicWriteAccess} -> allowsPublicWriteAccess) (\s@AccessControlList' {} a -> s {allowsPublicWriteAccess = a} :: AccessControlList)

instance Core.FromJSON AccessControlList where
  parseJSON =
    Core.withObject
      "AccessControlList"
      ( \x ->
          AccessControlList'
            Core.<$> (x Core..:? "allowsPublicReadAccess")
            Core.<*> (x Core..:? "allowsPublicWriteAccess")
      )

instance Core.Hashable AccessControlList

instance Core.NFData AccessControlList
