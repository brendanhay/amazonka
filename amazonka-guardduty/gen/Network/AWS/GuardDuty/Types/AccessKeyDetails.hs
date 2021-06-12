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
-- Module      : Network.AWS.GuardDuty.Types.AccessKeyDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccessKeyDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the access keys.
--
-- /See:/ 'newAccessKeyDetails' smart constructor.
data AccessKeyDetails = AccessKeyDetails'
  { -- | The principal ID of the user.
    principalId :: Core.Maybe Core.Text,
    -- | The type of the user.
    userType :: Core.Maybe Core.Text,
    -- | The access key ID of the user.
    accessKeyId :: Core.Maybe Core.Text,
    -- | The name of the user.
    userName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccessKeyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'accessKeyDetails_principalId' - The principal ID of the user.
--
-- 'userType', 'accessKeyDetails_userType' - The type of the user.
--
-- 'accessKeyId', 'accessKeyDetails_accessKeyId' - The access key ID of the user.
--
-- 'userName', 'accessKeyDetails_userName' - The name of the user.
newAccessKeyDetails ::
  AccessKeyDetails
newAccessKeyDetails =
  AccessKeyDetails'
    { principalId = Core.Nothing,
      userType = Core.Nothing,
      accessKeyId = Core.Nothing,
      userName = Core.Nothing
    }

-- | The principal ID of the user.
accessKeyDetails_principalId :: Lens.Lens' AccessKeyDetails (Core.Maybe Core.Text)
accessKeyDetails_principalId = Lens.lens (\AccessKeyDetails' {principalId} -> principalId) (\s@AccessKeyDetails' {} a -> s {principalId = a} :: AccessKeyDetails)

-- | The type of the user.
accessKeyDetails_userType :: Lens.Lens' AccessKeyDetails (Core.Maybe Core.Text)
accessKeyDetails_userType = Lens.lens (\AccessKeyDetails' {userType} -> userType) (\s@AccessKeyDetails' {} a -> s {userType = a} :: AccessKeyDetails)

-- | The access key ID of the user.
accessKeyDetails_accessKeyId :: Lens.Lens' AccessKeyDetails (Core.Maybe Core.Text)
accessKeyDetails_accessKeyId = Lens.lens (\AccessKeyDetails' {accessKeyId} -> accessKeyId) (\s@AccessKeyDetails' {} a -> s {accessKeyId = a} :: AccessKeyDetails)

-- | The name of the user.
accessKeyDetails_userName :: Lens.Lens' AccessKeyDetails (Core.Maybe Core.Text)
accessKeyDetails_userName = Lens.lens (\AccessKeyDetails' {userName} -> userName) (\s@AccessKeyDetails' {} a -> s {userName = a} :: AccessKeyDetails)

instance Core.FromJSON AccessKeyDetails where
  parseJSON =
    Core.withObject
      "AccessKeyDetails"
      ( \x ->
          AccessKeyDetails'
            Core.<$> (x Core..:? "principalId")
            Core.<*> (x Core..:? "userType")
            Core.<*> (x Core..:? "accessKeyId")
            Core.<*> (x Core..:? "userName")
      )

instance Core.Hashable AccessKeyDetails

instance Core.NFData AccessKeyDetails
