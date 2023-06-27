{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.UpgradeProfileVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrade a profile.
module Amazonka.WellArchitected.UpgradeProfileVersion
  ( -- * Creating a Request
    UpgradeProfileVersion (..),
    newUpgradeProfileVersion,

    -- * Request Lenses
    upgradeProfileVersion_clientRequestToken,
    upgradeProfileVersion_milestoneName,
    upgradeProfileVersion_workloadId,
    upgradeProfileVersion_profileArn,

    -- * Destructuring the Response
    UpgradeProfileVersionResponse (..),
    newUpgradeProfileVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newUpgradeProfileVersion' smart constructor.
data UpgradeProfileVersion = UpgradeProfileVersion'
  { clientRequestToken :: Prelude.Maybe Prelude.Text,
    milestoneName :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Text,
    -- | The profile ARN.
    profileArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeProfileVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'upgradeProfileVersion_clientRequestToken' - Undocumented member.
--
-- 'milestoneName', 'upgradeProfileVersion_milestoneName' - Undocumented member.
--
-- 'workloadId', 'upgradeProfileVersion_workloadId' - Undocumented member.
--
-- 'profileArn', 'upgradeProfileVersion_profileArn' - The profile ARN.
newUpgradeProfileVersion ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'profileArn'
  Prelude.Text ->
  UpgradeProfileVersion
newUpgradeProfileVersion pWorkloadId_ pProfileArn_ =
  UpgradeProfileVersion'
    { clientRequestToken =
        Prelude.Nothing,
      milestoneName = Prelude.Nothing,
      workloadId = pWorkloadId_,
      profileArn = pProfileArn_
    }

-- | Undocumented member.
upgradeProfileVersion_clientRequestToken :: Lens.Lens' UpgradeProfileVersion (Prelude.Maybe Prelude.Text)
upgradeProfileVersion_clientRequestToken = Lens.lens (\UpgradeProfileVersion' {clientRequestToken} -> clientRequestToken) (\s@UpgradeProfileVersion' {} a -> s {clientRequestToken = a} :: UpgradeProfileVersion)

-- | Undocumented member.
upgradeProfileVersion_milestoneName :: Lens.Lens' UpgradeProfileVersion (Prelude.Maybe Prelude.Text)
upgradeProfileVersion_milestoneName = Lens.lens (\UpgradeProfileVersion' {milestoneName} -> milestoneName) (\s@UpgradeProfileVersion' {} a -> s {milestoneName = a} :: UpgradeProfileVersion)

-- | Undocumented member.
upgradeProfileVersion_workloadId :: Lens.Lens' UpgradeProfileVersion Prelude.Text
upgradeProfileVersion_workloadId = Lens.lens (\UpgradeProfileVersion' {workloadId} -> workloadId) (\s@UpgradeProfileVersion' {} a -> s {workloadId = a} :: UpgradeProfileVersion)

-- | The profile ARN.
upgradeProfileVersion_profileArn :: Lens.Lens' UpgradeProfileVersion Prelude.Text
upgradeProfileVersion_profileArn = Lens.lens (\UpgradeProfileVersion' {profileArn} -> profileArn) (\s@UpgradeProfileVersion' {} a -> s {profileArn = a} :: UpgradeProfileVersion)

instance Core.AWSRequest UpgradeProfileVersion where
  type
    AWSResponse UpgradeProfileVersion =
      UpgradeProfileVersionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpgradeProfileVersionResponse'

instance Prelude.Hashable UpgradeProfileVersion where
  hashWithSalt _salt UpgradeProfileVersion' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` milestoneName
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` profileArn

instance Prelude.NFData UpgradeProfileVersion where
  rnf UpgradeProfileVersion' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf milestoneName
      `Prelude.seq` Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf profileArn

instance Data.ToHeaders UpgradeProfileVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpgradeProfileVersion where
  toJSON UpgradeProfileVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("MilestoneName" Data..=) Prelude.<$> milestoneName
          ]
      )

instance Data.ToPath UpgradeProfileVersion where
  toPath UpgradeProfileVersion' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/profiles/",
        Data.toBS profileArn,
        "/upgrade"
      ]

instance Data.ToQuery UpgradeProfileVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpgradeProfileVersionResponse' smart constructor.
data UpgradeProfileVersionResponse = UpgradeProfileVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeProfileVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpgradeProfileVersionResponse ::
  UpgradeProfileVersionResponse
newUpgradeProfileVersionResponse =
  UpgradeProfileVersionResponse'

instance Prelude.NFData UpgradeProfileVersionResponse where
  rnf _ = ()
