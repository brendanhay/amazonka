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
-- Module      : Network.AWS.CognitoIdentity.MergeDeveloperIdentities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Merges two users having different @IdentityId@s, existing in the same
-- identity pool, and identified by the same developer provider. You can
-- use this action to request that discrete users be merged and identified
-- as a single user in the Cognito environment. Cognito associates the
-- given source user (@SourceUserIdentifier@) with the @IdentityId@ of the
-- @DestinationUserIdentifier@. Only developer-authenticated users can be
-- merged. If the users to be merged are associated with the same public
-- provider, but as two different users, an exception will be thrown.
--
-- The number of linked logins is limited to 20. So, the number of linked
-- logins for the source user, @SourceUserIdentifier@, and the destination
-- user, @DestinationUserIdentifier@, together should not be larger than
-- 20. Otherwise, an exception will be thrown.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.MergeDeveloperIdentities
  ( -- * Creating a Request
    MergeDeveloperIdentities (..),
    newMergeDeveloperIdentities,

    -- * Request Lenses
    mergeDeveloperIdentities_sourceUserIdentifier,
    mergeDeveloperIdentities_destinationUserIdentifier,
    mergeDeveloperIdentities_developerProviderName,
    mergeDeveloperIdentities_identityPoolId,

    -- * Destructuring the Response
    MergeDeveloperIdentitiesResponse (..),
    newMergeDeveloperIdentitiesResponse,

    -- * Response Lenses
    mergeDeveloperIdentitiesResponse_identityId,
    mergeDeveloperIdentitiesResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @MergeDeveloperIdentities@ action.
--
-- /See:/ 'newMergeDeveloperIdentities' smart constructor.
data MergeDeveloperIdentities = MergeDeveloperIdentities'
  { -- | User identifier for the source user. The value should be a
    -- @DeveloperUserIdentifier@.
    sourceUserIdentifier :: Prelude.Text,
    -- | User identifier for the destination user. The value should be a
    -- @DeveloperUserIdentifier@.
    destinationUserIdentifier :: Prelude.Text,
    -- | The \"domain\" by which Cognito will refer to your users. This is a
    -- (pseudo) domain name that you provide while creating an identity pool.
    -- This name acts as a placeholder that allows your backend and the Cognito
    -- service to communicate about the developer provider. For the
    -- @DeveloperProviderName@, you can use letters as well as period (.),
    -- underscore (_), and dash (-).
    developerProviderName :: Prelude.Text,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MergeDeveloperIdentities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceUserIdentifier', 'mergeDeveloperIdentities_sourceUserIdentifier' - User identifier for the source user. The value should be a
-- @DeveloperUserIdentifier@.
--
-- 'destinationUserIdentifier', 'mergeDeveloperIdentities_destinationUserIdentifier' - User identifier for the destination user. The value should be a
-- @DeveloperUserIdentifier@.
--
-- 'developerProviderName', 'mergeDeveloperIdentities_developerProviderName' - The \"domain\" by which Cognito will refer to your users. This is a
-- (pseudo) domain name that you provide while creating an identity pool.
-- This name acts as a placeholder that allows your backend and the Cognito
-- service to communicate about the developer provider. For the
-- @DeveloperProviderName@, you can use letters as well as period (.),
-- underscore (_), and dash (-).
--
-- 'identityPoolId', 'mergeDeveloperIdentities_identityPoolId' - An identity pool ID in the format REGION:GUID.
newMergeDeveloperIdentities ::
  -- | 'sourceUserIdentifier'
  Prelude.Text ->
  -- | 'destinationUserIdentifier'
  Prelude.Text ->
  -- | 'developerProviderName'
  Prelude.Text ->
  -- | 'identityPoolId'
  Prelude.Text ->
  MergeDeveloperIdentities
newMergeDeveloperIdentities
  pSourceUserIdentifier_
  pDestinationUserIdentifier_
  pDeveloperProviderName_
  pIdentityPoolId_ =
    MergeDeveloperIdentities'
      { sourceUserIdentifier =
          pSourceUserIdentifier_,
        destinationUserIdentifier =
          pDestinationUserIdentifier_,
        developerProviderName = pDeveloperProviderName_,
        identityPoolId = pIdentityPoolId_
      }

-- | User identifier for the source user. The value should be a
-- @DeveloperUserIdentifier@.
mergeDeveloperIdentities_sourceUserIdentifier :: Lens.Lens' MergeDeveloperIdentities Prelude.Text
mergeDeveloperIdentities_sourceUserIdentifier = Lens.lens (\MergeDeveloperIdentities' {sourceUserIdentifier} -> sourceUserIdentifier) (\s@MergeDeveloperIdentities' {} a -> s {sourceUserIdentifier = a} :: MergeDeveloperIdentities)

-- | User identifier for the destination user. The value should be a
-- @DeveloperUserIdentifier@.
mergeDeveloperIdentities_destinationUserIdentifier :: Lens.Lens' MergeDeveloperIdentities Prelude.Text
mergeDeveloperIdentities_destinationUserIdentifier = Lens.lens (\MergeDeveloperIdentities' {destinationUserIdentifier} -> destinationUserIdentifier) (\s@MergeDeveloperIdentities' {} a -> s {destinationUserIdentifier = a} :: MergeDeveloperIdentities)

-- | The \"domain\" by which Cognito will refer to your users. This is a
-- (pseudo) domain name that you provide while creating an identity pool.
-- This name acts as a placeholder that allows your backend and the Cognito
-- service to communicate about the developer provider. For the
-- @DeveloperProviderName@, you can use letters as well as period (.),
-- underscore (_), and dash (-).
mergeDeveloperIdentities_developerProviderName :: Lens.Lens' MergeDeveloperIdentities Prelude.Text
mergeDeveloperIdentities_developerProviderName = Lens.lens (\MergeDeveloperIdentities' {developerProviderName} -> developerProviderName) (\s@MergeDeveloperIdentities' {} a -> s {developerProviderName = a} :: MergeDeveloperIdentities)

-- | An identity pool ID in the format REGION:GUID.
mergeDeveloperIdentities_identityPoolId :: Lens.Lens' MergeDeveloperIdentities Prelude.Text
mergeDeveloperIdentities_identityPoolId = Lens.lens (\MergeDeveloperIdentities' {identityPoolId} -> identityPoolId) (\s@MergeDeveloperIdentities' {} a -> s {identityPoolId = a} :: MergeDeveloperIdentities)

instance Prelude.AWSRequest MergeDeveloperIdentities where
  type
    Rs MergeDeveloperIdentities =
      MergeDeveloperIdentitiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          MergeDeveloperIdentitiesResponse'
            Prelude.<$> (x Prelude..?> "IdentityId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MergeDeveloperIdentities

instance Prelude.NFData MergeDeveloperIdentities

instance Prelude.ToHeaders MergeDeveloperIdentities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityService.MergeDeveloperIdentities" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON MergeDeveloperIdentities where
  toJSON MergeDeveloperIdentities' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SourceUserIdentifier"
                  Prelude..= sourceUserIdentifier
              ),
            Prelude.Just
              ( "DestinationUserIdentifier"
                  Prelude..= destinationUserIdentifier
              ),
            Prelude.Just
              ( "DeveloperProviderName"
                  Prelude..= developerProviderName
              ),
            Prelude.Just
              ("IdentityPoolId" Prelude..= identityPoolId)
          ]
      )

instance Prelude.ToPath MergeDeveloperIdentities where
  toPath = Prelude.const "/"

instance Prelude.ToQuery MergeDeveloperIdentities where
  toQuery = Prelude.const Prelude.mempty

-- | Returned in response to a successful @MergeDeveloperIdentities@ action.
--
-- /See:/ 'newMergeDeveloperIdentitiesResponse' smart constructor.
data MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MergeDeveloperIdentitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'mergeDeveloperIdentitiesResponse_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'httpStatus', 'mergeDeveloperIdentitiesResponse_httpStatus' - The response's http status code.
newMergeDeveloperIdentitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MergeDeveloperIdentitiesResponse
newMergeDeveloperIdentitiesResponse pHttpStatus_ =
  MergeDeveloperIdentitiesResponse'
    { identityId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier in the format REGION:GUID.
mergeDeveloperIdentitiesResponse_identityId :: Lens.Lens' MergeDeveloperIdentitiesResponse (Prelude.Maybe Prelude.Text)
mergeDeveloperIdentitiesResponse_identityId = Lens.lens (\MergeDeveloperIdentitiesResponse' {identityId} -> identityId) (\s@MergeDeveloperIdentitiesResponse' {} a -> s {identityId = a} :: MergeDeveloperIdentitiesResponse)

-- | The response's http status code.
mergeDeveloperIdentitiesResponse_httpStatus :: Lens.Lens' MergeDeveloperIdentitiesResponse Prelude.Int
mergeDeveloperIdentitiesResponse_httpStatus = Lens.lens (\MergeDeveloperIdentitiesResponse' {httpStatus} -> httpStatus) (\s@MergeDeveloperIdentitiesResponse' {} a -> s {httpStatus = a} :: MergeDeveloperIdentitiesResponse)

instance
  Prelude.NFData
    MergeDeveloperIdentitiesResponse
