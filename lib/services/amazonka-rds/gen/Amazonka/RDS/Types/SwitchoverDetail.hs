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
-- Module      : Amazonka.RDS.Types.SwitchoverDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.SwitchoverDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details about a blue\/green deployment.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newSwitchoverDetail' smart constructor.
data SwitchoverDetail = SwitchoverDetail'
  { -- | The Amazon Resource Name (ARN) of a resource in the blue environment.
    sourceMember :: Prelude.Maybe Prelude.Text,
    -- | The switchover status of a resource in a blue\/green deployment.
    --
    -- Values:
    --
    -- -   @preparing-for-switchover@ - The resource is being prepared to
    --     switch over.
    --
    -- -   @ready-for-switchover@ - The resource is ready to switch over.
    --
    -- -   @switchover-in-progress@ - The resource is being switched over.
    --
    -- -   @switchover-completed@ - The resource has been switched over.
    --
    -- -   @switchover-failed@ - The resource attempted to switch over but
    --     failed.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a resource in the green environment.
    targetMember :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SwitchoverDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceMember', 'switchoverDetail_sourceMember' - The Amazon Resource Name (ARN) of a resource in the blue environment.
--
-- 'status', 'switchoverDetail_status' - The switchover status of a resource in a blue\/green deployment.
--
-- Values:
--
-- -   @preparing-for-switchover@ - The resource is being prepared to
--     switch over.
--
-- -   @ready-for-switchover@ - The resource is ready to switch over.
--
-- -   @switchover-in-progress@ - The resource is being switched over.
--
-- -   @switchover-completed@ - The resource has been switched over.
--
-- -   @switchover-failed@ - The resource attempted to switch over but
--     failed.
--
-- 'targetMember', 'switchoverDetail_targetMember' - The Amazon Resource Name (ARN) of a resource in the green environment.
newSwitchoverDetail ::
  SwitchoverDetail
newSwitchoverDetail =
  SwitchoverDetail'
    { sourceMember = Prelude.Nothing,
      status = Prelude.Nothing,
      targetMember = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a resource in the blue environment.
switchoverDetail_sourceMember :: Lens.Lens' SwitchoverDetail (Prelude.Maybe Prelude.Text)
switchoverDetail_sourceMember = Lens.lens (\SwitchoverDetail' {sourceMember} -> sourceMember) (\s@SwitchoverDetail' {} a -> s {sourceMember = a} :: SwitchoverDetail)

-- | The switchover status of a resource in a blue\/green deployment.
--
-- Values:
--
-- -   @preparing-for-switchover@ - The resource is being prepared to
--     switch over.
--
-- -   @ready-for-switchover@ - The resource is ready to switch over.
--
-- -   @switchover-in-progress@ - The resource is being switched over.
--
-- -   @switchover-completed@ - The resource has been switched over.
--
-- -   @switchover-failed@ - The resource attempted to switch over but
--     failed.
switchoverDetail_status :: Lens.Lens' SwitchoverDetail (Prelude.Maybe Prelude.Text)
switchoverDetail_status = Lens.lens (\SwitchoverDetail' {status} -> status) (\s@SwitchoverDetail' {} a -> s {status = a} :: SwitchoverDetail)

-- | The Amazon Resource Name (ARN) of a resource in the green environment.
switchoverDetail_targetMember :: Lens.Lens' SwitchoverDetail (Prelude.Maybe Prelude.Text)
switchoverDetail_targetMember = Lens.lens (\SwitchoverDetail' {targetMember} -> targetMember) (\s@SwitchoverDetail' {} a -> s {targetMember = a} :: SwitchoverDetail)

instance Data.FromXML SwitchoverDetail where
  parseXML x =
    SwitchoverDetail'
      Prelude.<$> (x Data..@? "SourceMember")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "TargetMember")

instance Prelude.Hashable SwitchoverDetail where
  hashWithSalt _salt SwitchoverDetail' {..} =
    _salt
      `Prelude.hashWithSalt` sourceMember
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetMember

instance Prelude.NFData SwitchoverDetail where
  rnf SwitchoverDetail' {..} =
    Prelude.rnf sourceMember
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetMember
