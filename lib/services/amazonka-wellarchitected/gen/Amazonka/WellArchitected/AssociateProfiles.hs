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
-- Module      : Amazonka.WellArchitected.AssociateProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate a profile with a workload.
module Amazonka.WellArchitected.AssociateProfiles
  ( -- * Creating a Request
    AssociateProfiles (..),
    newAssociateProfiles,

    -- * Request Lenses
    associateProfiles_workloadId,
    associateProfiles_profileArns,

    -- * Destructuring the Response
    AssociateProfilesResponse (..),
    newAssociateProfilesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newAssociateProfiles' smart constructor.
data AssociateProfiles = AssociateProfiles'
  { workloadId :: Prelude.Text,
    -- | The list of profile ARNs to associate with the workload.
    profileArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadId', 'associateProfiles_workloadId' - Undocumented member.
--
-- 'profileArns', 'associateProfiles_profileArns' - The list of profile ARNs to associate with the workload.
newAssociateProfiles ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'profileArns'
  Prelude.NonEmpty Prelude.Text ->
  AssociateProfiles
newAssociateProfiles pWorkloadId_ pProfileArns_ =
  AssociateProfiles'
    { workloadId = pWorkloadId_,
      profileArns = Lens.coerced Lens.# pProfileArns_
    }

-- | Undocumented member.
associateProfiles_workloadId :: Lens.Lens' AssociateProfiles Prelude.Text
associateProfiles_workloadId = Lens.lens (\AssociateProfiles' {workloadId} -> workloadId) (\s@AssociateProfiles' {} a -> s {workloadId = a} :: AssociateProfiles)

-- | The list of profile ARNs to associate with the workload.
associateProfiles_profileArns :: Lens.Lens' AssociateProfiles (Prelude.NonEmpty Prelude.Text)
associateProfiles_profileArns = Lens.lens (\AssociateProfiles' {profileArns} -> profileArns) (\s@AssociateProfiles' {} a -> s {profileArns = a} :: AssociateProfiles) Prelude.. Lens.coerced

instance Core.AWSRequest AssociateProfiles where
  type
    AWSResponse AssociateProfiles =
      AssociateProfilesResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull AssociateProfilesResponse'

instance Prelude.Hashable AssociateProfiles where
  hashWithSalt _salt AssociateProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` profileArns

instance Prelude.NFData AssociateProfiles where
  rnf AssociateProfiles' {..} =
    Prelude.rnf workloadId
      `Prelude.seq` Prelude.rnf profileArns

instance Data.ToHeaders AssociateProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateProfiles where
  toJSON AssociateProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProfileArns" Data..= profileArns)]
      )

instance Data.ToPath AssociateProfiles where
  toPath AssociateProfiles' {..} =
    Prelude.mconcat
      [ "/workloads/",
        Data.toBS workloadId,
        "/associateProfiles"
      ]

instance Data.ToQuery AssociateProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateProfilesResponse' smart constructor.
data AssociateProfilesResponse = AssociateProfilesResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateProfilesResponse ::
  AssociateProfilesResponse
newAssociateProfilesResponse =
  AssociateProfilesResponse'

instance Prelude.NFData AssociateProfilesResponse where
  rnf _ = ()
