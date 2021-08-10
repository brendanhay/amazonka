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
-- Module      : Network.AWS.CognitoIdentity.UnlinkIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a federated identity from an existing account. Unlinked logins
-- will be considered new identities next time they are seen. Removing the
-- last linked login will make this identity inaccessible.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.UnlinkIdentity
  ( -- * Creating a Request
    UnlinkIdentity (..),
    newUnlinkIdentity,

    -- * Request Lenses
    unlinkIdentity_identityId,
    unlinkIdentity_logins,
    unlinkIdentity_loginsToRemove,

    -- * Destructuring the Response
    UnlinkIdentityResponse (..),
    newUnlinkIdentityResponse,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the UnlinkIdentity action.
--
-- /See:/ 'newUnlinkIdentity' smart constructor.
data UnlinkIdentity = UnlinkIdentity'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Text,
    -- | A set of optional name-value pairs that map provider names to provider
    -- tokens.
    logins :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Provider names to unlink from this identity.
    loginsToRemove :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlinkIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'unlinkIdentity_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'logins', 'unlinkIdentity_logins' - A set of optional name-value pairs that map provider names to provider
-- tokens.
--
-- 'loginsToRemove', 'unlinkIdentity_loginsToRemove' - Provider names to unlink from this identity.
newUnlinkIdentity ::
  -- | 'identityId'
  Prelude.Text ->
  UnlinkIdentity
newUnlinkIdentity pIdentityId_ =
  UnlinkIdentity'
    { identityId = pIdentityId_,
      logins = Prelude.mempty,
      loginsToRemove = Prelude.mempty
    }

-- | A unique identifier in the format REGION:GUID.
unlinkIdentity_identityId :: Lens.Lens' UnlinkIdentity Prelude.Text
unlinkIdentity_identityId = Lens.lens (\UnlinkIdentity' {identityId} -> identityId) (\s@UnlinkIdentity' {} a -> s {identityId = a} :: UnlinkIdentity)

-- | A set of optional name-value pairs that map provider names to provider
-- tokens.
unlinkIdentity_logins :: Lens.Lens' UnlinkIdentity (Prelude.HashMap Prelude.Text Prelude.Text)
unlinkIdentity_logins = Lens.lens (\UnlinkIdentity' {logins} -> logins) (\s@UnlinkIdentity' {} a -> s {logins = a} :: UnlinkIdentity) Prelude.. Lens._Coerce

-- | Provider names to unlink from this identity.
unlinkIdentity_loginsToRemove :: Lens.Lens' UnlinkIdentity [Prelude.Text]
unlinkIdentity_loginsToRemove = Lens.lens (\UnlinkIdentity' {loginsToRemove} -> loginsToRemove) (\s@UnlinkIdentity' {} a -> s {loginsToRemove = a} :: UnlinkIdentity) Prelude.. Lens._Coerce

instance Core.AWSRequest UnlinkIdentity where
  type
    AWSResponse UnlinkIdentity =
      UnlinkIdentityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UnlinkIdentityResponse'

instance Prelude.Hashable UnlinkIdentity

instance Prelude.NFData UnlinkIdentity

instance Core.ToHeaders UnlinkIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.UnlinkIdentity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UnlinkIdentity where
  toJSON UnlinkIdentity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IdentityId" Core..= identityId),
            Prelude.Just ("Logins" Core..= logins),
            Prelude.Just
              ("LoginsToRemove" Core..= loginsToRemove)
          ]
      )

instance Core.ToPath UnlinkIdentity where
  toPath = Prelude.const "/"

instance Core.ToQuery UnlinkIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnlinkIdentityResponse' smart constructor.
data UnlinkIdentityResponse = UnlinkIdentityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlinkIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnlinkIdentityResponse ::
  UnlinkIdentityResponse
newUnlinkIdentityResponse = UnlinkIdentityResponse'

instance Prelude.NFData UnlinkIdentityResponse
