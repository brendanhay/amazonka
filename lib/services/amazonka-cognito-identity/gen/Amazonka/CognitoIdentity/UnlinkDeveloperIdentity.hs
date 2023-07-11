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
-- Module      : Amazonka.CognitoIdentity.UnlinkDeveloperIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a @DeveloperUserIdentifier@ from an existing identity. Unlinked
-- developer users will be considered new identities next time they are
-- seen. If, for a given Cognito identity, you remove all federated
-- identities as well as the developer user identifier, the Cognito
-- identity becomes inaccessible.
--
-- You must use AWS Developer credentials to call this API.
module Amazonka.CognitoIdentity.UnlinkDeveloperIdentity
  ( -- * Creating a Request
    UnlinkDeveloperIdentity (..),
    newUnlinkDeveloperIdentity,

    -- * Request Lenses
    unlinkDeveloperIdentity_identityId,
    unlinkDeveloperIdentity_identityPoolId,
    unlinkDeveloperIdentity_developerProviderName,
    unlinkDeveloperIdentity_developerUserIdentifier,

    -- * Destructuring the Response
    UnlinkDeveloperIdentityResponse (..),
    newUnlinkDeveloperIdentityResponse,
  )
where

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the @UnlinkDeveloperIdentity@ action.
--
-- /See:/ 'newUnlinkDeveloperIdentity' smart constructor.
data UnlinkDeveloperIdentity = UnlinkDeveloperIdentity'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Text,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text,
    -- | The \"domain\" by which Cognito will refer to your users.
    developerProviderName :: Prelude.Text,
    -- | A unique ID used by your backend authentication process to identify a
    -- user.
    developerUserIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlinkDeveloperIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'unlinkDeveloperIdentity_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'identityPoolId', 'unlinkDeveloperIdentity_identityPoolId' - An identity pool ID in the format REGION:GUID.
--
-- 'developerProviderName', 'unlinkDeveloperIdentity_developerProviderName' - The \"domain\" by which Cognito will refer to your users.
--
-- 'developerUserIdentifier', 'unlinkDeveloperIdentity_developerUserIdentifier' - A unique ID used by your backend authentication process to identify a
-- user.
newUnlinkDeveloperIdentity ::
  -- | 'identityId'
  Prelude.Text ->
  -- | 'identityPoolId'
  Prelude.Text ->
  -- | 'developerProviderName'
  Prelude.Text ->
  -- | 'developerUserIdentifier'
  Prelude.Text ->
  UnlinkDeveloperIdentity
newUnlinkDeveloperIdentity
  pIdentityId_
  pIdentityPoolId_
  pDeveloperProviderName_
  pDeveloperUserIdentifier_ =
    UnlinkDeveloperIdentity'
      { identityId = pIdentityId_,
        identityPoolId = pIdentityPoolId_,
        developerProviderName = pDeveloperProviderName_,
        developerUserIdentifier =
          pDeveloperUserIdentifier_
      }

-- | A unique identifier in the format REGION:GUID.
unlinkDeveloperIdentity_identityId :: Lens.Lens' UnlinkDeveloperIdentity Prelude.Text
unlinkDeveloperIdentity_identityId = Lens.lens (\UnlinkDeveloperIdentity' {identityId} -> identityId) (\s@UnlinkDeveloperIdentity' {} a -> s {identityId = a} :: UnlinkDeveloperIdentity)

-- | An identity pool ID in the format REGION:GUID.
unlinkDeveloperIdentity_identityPoolId :: Lens.Lens' UnlinkDeveloperIdentity Prelude.Text
unlinkDeveloperIdentity_identityPoolId = Lens.lens (\UnlinkDeveloperIdentity' {identityPoolId} -> identityPoolId) (\s@UnlinkDeveloperIdentity' {} a -> s {identityPoolId = a} :: UnlinkDeveloperIdentity)

-- | The \"domain\" by which Cognito will refer to your users.
unlinkDeveloperIdentity_developerProviderName :: Lens.Lens' UnlinkDeveloperIdentity Prelude.Text
unlinkDeveloperIdentity_developerProviderName = Lens.lens (\UnlinkDeveloperIdentity' {developerProviderName} -> developerProviderName) (\s@UnlinkDeveloperIdentity' {} a -> s {developerProviderName = a} :: UnlinkDeveloperIdentity)

-- | A unique ID used by your backend authentication process to identify a
-- user.
unlinkDeveloperIdentity_developerUserIdentifier :: Lens.Lens' UnlinkDeveloperIdentity Prelude.Text
unlinkDeveloperIdentity_developerUserIdentifier = Lens.lens (\UnlinkDeveloperIdentity' {developerUserIdentifier} -> developerUserIdentifier) (\s@UnlinkDeveloperIdentity' {} a -> s {developerUserIdentifier = a} :: UnlinkDeveloperIdentity)

instance Core.AWSRequest UnlinkDeveloperIdentity where
  type
    AWSResponse UnlinkDeveloperIdentity =
      UnlinkDeveloperIdentityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UnlinkDeveloperIdentityResponse'

instance Prelude.Hashable UnlinkDeveloperIdentity where
  hashWithSalt _salt UnlinkDeveloperIdentity' {..} =
    _salt
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` developerProviderName
      `Prelude.hashWithSalt` developerUserIdentifier

instance Prelude.NFData UnlinkDeveloperIdentity where
  rnf UnlinkDeveloperIdentity' {..} =
    Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf developerProviderName
      `Prelude.seq` Prelude.rnf developerUserIdentifier

instance Data.ToHeaders UnlinkDeveloperIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityService.UnlinkDeveloperIdentity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnlinkDeveloperIdentity where
  toJSON UnlinkDeveloperIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IdentityId" Data..= identityId),
            Prelude.Just
              ("IdentityPoolId" Data..= identityPoolId),
            Prelude.Just
              ( "DeveloperProviderName"
                  Data..= developerProviderName
              ),
            Prelude.Just
              ( "DeveloperUserIdentifier"
                  Data..= developerUserIdentifier
              )
          ]
      )

instance Data.ToPath UnlinkDeveloperIdentity where
  toPath = Prelude.const "/"

instance Data.ToQuery UnlinkDeveloperIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnlinkDeveloperIdentityResponse' smart constructor.
data UnlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlinkDeveloperIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnlinkDeveloperIdentityResponse ::
  UnlinkDeveloperIdentityResponse
newUnlinkDeveloperIdentityResponse =
  UnlinkDeveloperIdentityResponse'

instance
  Prelude.NFData
    UnlinkDeveloperIdentityResponse
  where
  rnf _ = ()
