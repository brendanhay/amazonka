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
-- Module      : Amazonka.SageMaker.Types.RStudioServerProAppSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RStudioServerProAppSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RStudioServerProAccessStatus
import Amazonka.SageMaker.Types.RStudioServerProUserGroup

-- | A collection of settings that configure user interaction with the
-- @RStudioServerPro@ app. @RStudioServerProAppSettings@ cannot be updated.
-- The @RStudioServerPro@ app must be deleted and a new one created to make
-- any changes.
--
-- /See:/ 'newRStudioServerProAppSettings' smart constructor.
data RStudioServerProAppSettings = RStudioServerProAppSettings'
  { -- | Indicates whether the current user has access to the @RStudioServerPro@
    -- app.
    accessStatus :: Prelude.Maybe RStudioServerProAccessStatus,
    -- | The level of permissions that the user has within the @RStudioServerPro@
    -- app. This value defaults to \`User\`. The \`Admin\` value allows the
    -- user access to the RStudio Administrative Dashboard.
    userGroup :: Prelude.Maybe RStudioServerProUserGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RStudioServerProAppSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessStatus', 'rStudioServerProAppSettings_accessStatus' - Indicates whether the current user has access to the @RStudioServerPro@
-- app.
--
-- 'userGroup', 'rStudioServerProAppSettings_userGroup' - The level of permissions that the user has within the @RStudioServerPro@
-- app. This value defaults to \`User\`. The \`Admin\` value allows the
-- user access to the RStudio Administrative Dashboard.
newRStudioServerProAppSettings ::
  RStudioServerProAppSettings
newRStudioServerProAppSettings =
  RStudioServerProAppSettings'
    { accessStatus =
        Prelude.Nothing,
      userGroup = Prelude.Nothing
    }

-- | Indicates whether the current user has access to the @RStudioServerPro@
-- app.
rStudioServerProAppSettings_accessStatus :: Lens.Lens' RStudioServerProAppSettings (Prelude.Maybe RStudioServerProAccessStatus)
rStudioServerProAppSettings_accessStatus = Lens.lens (\RStudioServerProAppSettings' {accessStatus} -> accessStatus) (\s@RStudioServerProAppSettings' {} a -> s {accessStatus = a} :: RStudioServerProAppSettings)

-- | The level of permissions that the user has within the @RStudioServerPro@
-- app. This value defaults to \`User\`. The \`Admin\` value allows the
-- user access to the RStudio Administrative Dashboard.
rStudioServerProAppSettings_userGroup :: Lens.Lens' RStudioServerProAppSettings (Prelude.Maybe RStudioServerProUserGroup)
rStudioServerProAppSettings_userGroup = Lens.lens (\RStudioServerProAppSettings' {userGroup} -> userGroup) (\s@RStudioServerProAppSettings' {} a -> s {userGroup = a} :: RStudioServerProAppSettings)

instance Core.FromJSON RStudioServerProAppSettings where
  parseJSON =
    Core.withObject
      "RStudioServerProAppSettings"
      ( \x ->
          RStudioServerProAppSettings'
            Prelude.<$> (x Core..:? "AccessStatus")
            Prelude.<*> (x Core..:? "UserGroup")
      )

instance Prelude.Hashable RStudioServerProAppSettings where
  hashWithSalt _salt RStudioServerProAppSettings' {..} =
    _salt `Prelude.hashWithSalt` accessStatus
      `Prelude.hashWithSalt` userGroup

instance Prelude.NFData RStudioServerProAppSettings where
  rnf RStudioServerProAppSettings' {..} =
    Prelude.rnf accessStatus
      `Prelude.seq` Prelude.rnf userGroup

instance Core.ToJSON RStudioServerProAppSettings where
  toJSON RStudioServerProAppSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AccessStatus" Core..=) Prelude.<$> accessStatus,
            ("UserGroup" Core..=) Prelude.<$> userGroup
          ]
      )
