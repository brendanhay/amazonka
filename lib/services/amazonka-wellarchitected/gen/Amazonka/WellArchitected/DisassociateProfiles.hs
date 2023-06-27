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
-- Module      : Amazonka.WellArchitected.DisassociateProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate a profile from a workload.
module Amazonka.WellArchitected.DisassociateProfiles
  ( -- * Creating a Request
    DisassociateProfiles (..),
    newDisassociateProfiles,

    -- * Request Lenses
    disassociateProfiles_workloadId,
    disassociateProfiles_profileArns,

    -- * Destructuring the Response
    DisassociateProfilesResponse (..),
    newDisassociateProfilesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newDisassociateProfiles' smart constructor.
data DisassociateProfiles = DisassociateProfiles'
  { workloadId :: Prelude.Text,
    -- | The list of profile ARNs to disassociate from the workload.
    profileArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadId', 'disassociateProfiles_workloadId' - Undocumented member.
--
-- 'profileArns', 'disassociateProfiles_profileArns' - The list of profile ARNs to disassociate from the workload.
newDisassociateProfiles ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'profileArns'
  Prelude.NonEmpty Prelude.Text ->
  DisassociateProfiles
newDisassociateProfiles pWorkloadId_ pProfileArns_ =
  DisassociateProfiles'
    { workloadId = pWorkloadId_,
      profileArns = Lens.coerced Lens.# pProfileArns_
    }

-- | Undocumented member.
disassociateProfiles_workloadId :: Lens.Lens' DisassociateProfiles Prelude.Text
disassociateProfiles_workloadId = Lens.lens (\DisassociateProfiles' {workloadId} -> workloadId) (\s@DisassociateProfiles' {} a -> s {workloadId = a} :: DisassociateProfiles)

-- | The list of profile ARNs to disassociate from the workload.
disassociateProfiles_profileArns :: Lens.Lens' DisassociateProfiles (Prelude.NonEmpty Prelude.Text)
disassociateProfiles_profileArns = Lens.lens (\DisassociateProfiles' {profileArns} -> profileArns) (\s@DisassociateProfiles' {} a -> s {profileArns = a} :: DisassociateProfiles) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateProfiles where
  type
    AWSResponse DisassociateProfiles =
      DisassociateProfilesResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull DisassociateProfilesResponse'

instance Prelude.Hashable DisassociateProfiles where
  hashWithSalt _salt DisassociateProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` profileArns

instance Prelude.NFData DisassociateProfiles where
  rnf DisassociateProfiles' {..} =
    Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf profileArns

instance Data.ToHeaders DisassociateProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateProfiles where
  toJSON DisassociateProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProfileArns" Data..= profileArns)]
      )

instance Data.ToPath DisassociateProfiles where
  toPath DisassociateProfiles' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/disassociateProfiles"
      ]

instance Data.ToQuery DisassociateProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateProfilesResponse' smart constructor.
data DisassociateProfilesResponse = DisassociateProfilesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateProfilesResponse ::
  DisassociateProfilesResponse
newDisassociateProfilesResponse =
  DisassociateProfilesResponse'

instance Prelude.NFData DisassociateProfilesResponse where
  rnf _ = ()
