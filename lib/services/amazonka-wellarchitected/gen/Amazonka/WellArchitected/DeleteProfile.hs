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
-- Module      : Amazonka.WellArchitected.DeleteProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a profile.
--
-- __Disclaimer__
--
-- By sharing your profile with other Amazon Web Services accounts, you
-- acknowledge that Amazon Web Services will make your profile available to
-- those other accounts. Those other accounts may continue to access and
-- use your shared profile even if you delete the profile from your own
-- Amazon Web Services account or terminate your Amazon Web Services
-- account.
module Amazonka.WellArchitected.DeleteProfile
  ( -- * Creating a Request
    DeleteProfile (..),
    newDeleteProfile,

    -- * Request Lenses
    deleteProfile_profileArn,
    deleteProfile_clientRequestToken,

    -- * Destructuring the Response
    DeleteProfileResponse (..),
    newDeleteProfileResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newDeleteProfile' smart constructor.
data DeleteProfile = DeleteProfile'
  { -- | The profile ARN.
    profileArn :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileArn', 'deleteProfile_profileArn' - The profile ARN.
--
-- 'clientRequestToken', 'deleteProfile_clientRequestToken' - Undocumented member.
newDeleteProfile ::
  -- | 'profileArn'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  DeleteProfile
newDeleteProfile pProfileArn_ pClientRequestToken_ =
  DeleteProfile'
    { profileArn = pProfileArn_,
      clientRequestToken = pClientRequestToken_
    }

-- | The profile ARN.
deleteProfile_profileArn :: Lens.Lens' DeleteProfile Prelude.Text
deleteProfile_profileArn = Lens.lens (\DeleteProfile' {profileArn} -> profileArn) (\s@DeleteProfile' {} a -> s {profileArn = a} :: DeleteProfile)

-- | Undocumented member.
deleteProfile_clientRequestToken :: Lens.Lens' DeleteProfile Prelude.Text
deleteProfile_clientRequestToken = Lens.lens (\DeleteProfile' {clientRequestToken} -> clientRequestToken) (\s@DeleteProfile' {} a -> s {clientRequestToken = a} :: DeleteProfile)

instance Core.AWSRequest DeleteProfile where
  type
    AWSResponse DeleteProfile =
      DeleteProfileResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteProfileResponse'

instance Prelude.Hashable DeleteProfile where
  hashWithSalt _salt DeleteProfile' {..} =
    _salt
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData DeleteProfile where
  rnf DeleteProfile' {..} =
    Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Data.ToHeaders DeleteProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteProfile where
  toPath DeleteProfile' {..} =
    Prelude.mconcat
      ["/profiles/", Data.toBS profileArn]

instance Data.ToQuery DeleteProfile where
  toQuery DeleteProfile' {..} =
    Prelude.mconcat
      ["ClientRequestToken" Data.=: clientRequestToken]

-- | /See:/ 'newDeleteProfileResponse' smart constructor.
data DeleteProfileResponse = DeleteProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProfileResponse ::
  DeleteProfileResponse
newDeleteProfileResponse = DeleteProfileResponse'

instance Prelude.NFData DeleteProfileResponse where
  rnf _ = ()
