{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.CognitoIdentity.UnlinkDeveloperIdentity
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

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UnlinkDeveloperIdentity where
  type
    Rs UnlinkDeveloperIdentity =
      UnlinkDeveloperIdentityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UnlinkDeveloperIdentityResponse'

instance Prelude.Hashable UnlinkDeveloperIdentity

instance Prelude.NFData UnlinkDeveloperIdentity

instance Prelude.ToHeaders UnlinkDeveloperIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityService.UnlinkDeveloperIdentity" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UnlinkDeveloperIdentity where
  toJSON UnlinkDeveloperIdentity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IdentityId" Prelude..= identityId),
            Prelude.Just
              ("IdentityPoolId" Prelude..= identityPoolId),
            Prelude.Just
              ( "DeveloperProviderName"
                  Prelude..= developerProviderName
              ),
            Prelude.Just
              ( "DeveloperUserIdentifier"
                  Prelude..= developerUserIdentifier
              )
          ]
      )

instance Prelude.ToPath UnlinkDeveloperIdentity where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UnlinkDeveloperIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnlinkDeveloperIdentityResponse' smart constructor.
data UnlinkDeveloperIdentityResponse = UnlinkDeveloperIdentityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
