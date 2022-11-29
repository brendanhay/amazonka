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
-- Module      : Amazonka.Signer.CancelSigningProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the state of an @ACTIVE@ signing profile to @CANCELED@. A
-- canceled profile is still viewable with the @ListSigningProfiles@
-- operation, but it cannot perform new signing jobs, and is deleted two
-- years after cancelation.
module Amazonka.Signer.CancelSigningProfile
  ( -- * Creating a Request
    CancelSigningProfile (..),
    newCancelSigningProfile,

    -- * Request Lenses
    cancelSigningProfile_profileName,

    -- * Destructuring the Response
    CancelSigningProfileResponse (..),
    newCancelSigningProfileResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newCancelSigningProfile' smart constructor.
data CancelSigningProfile = CancelSigningProfile'
  { -- | The name of the signing profile to be canceled.
    profileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSigningProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileName', 'cancelSigningProfile_profileName' - The name of the signing profile to be canceled.
newCancelSigningProfile ::
  -- | 'profileName'
  Prelude.Text ->
  CancelSigningProfile
newCancelSigningProfile pProfileName_ =
  CancelSigningProfile' {profileName = pProfileName_}

-- | The name of the signing profile to be canceled.
cancelSigningProfile_profileName :: Lens.Lens' CancelSigningProfile Prelude.Text
cancelSigningProfile_profileName = Lens.lens (\CancelSigningProfile' {profileName} -> profileName) (\s@CancelSigningProfile' {} a -> s {profileName = a} :: CancelSigningProfile)

instance Core.AWSRequest CancelSigningProfile where
  type
    AWSResponse CancelSigningProfile =
      CancelSigningProfileResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull CancelSigningProfileResponse'

instance Prelude.Hashable CancelSigningProfile where
  hashWithSalt _salt CancelSigningProfile' {..} =
    _salt `Prelude.hashWithSalt` profileName

instance Prelude.NFData CancelSigningProfile where
  rnf CancelSigningProfile' {..} =
    Prelude.rnf profileName

instance Core.ToHeaders CancelSigningProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath CancelSigningProfile where
  toPath CancelSigningProfile' {..} =
    Prelude.mconcat
      ["/signing-profiles/", Core.toBS profileName]

instance Core.ToQuery CancelSigningProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelSigningProfileResponse' smart constructor.
data CancelSigningProfileResponse = CancelSigningProfileResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSigningProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelSigningProfileResponse ::
  CancelSigningProfileResponse
newCancelSigningProfileResponse =
  CancelSigningProfileResponse'

instance Prelude.NFData CancelSigningProfileResponse where
  rnf _ = ()
