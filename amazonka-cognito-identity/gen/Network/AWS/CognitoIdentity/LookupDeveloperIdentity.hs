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
-- Module      : Network.AWS.CognitoIdentity.LookupDeveloperIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @IdentityID@ associated with a @DeveloperUserIdentifier@
-- or the list of @DeveloperUserIdentifier@ values associated with an
-- @IdentityId@ for an existing identity. Either @IdentityID@ or
-- @DeveloperUserIdentifier@ must not be null. If you supply only one of
-- these values, the other value will be searched in the database and
-- returned as a part of the response. If you supply both,
-- @DeveloperUserIdentifier@ will be matched against @IdentityID@. If the
-- values are verified against the database, the response returns both
-- values and is the same as the request. Otherwise a
-- @ResourceConflictException@ is thrown.
--
-- @LookupDeveloperIdentity@ is intended for low-throughput control plane
-- operations: for example, to enable customer service to locate an
-- identity ID by username. If you are using it for higher-volume
-- operations such as user authentication, your requests are likely to be
-- throttled. GetOpenIdTokenForDeveloperIdentity is a better option for
-- higher-volume operations for user authentication.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.LookupDeveloperIdentity
  ( -- * Creating a Request
    LookupDeveloperIdentity (..),
    newLookupDeveloperIdentity,

    -- * Request Lenses
    lookupDeveloperIdentity_nextToken,
    lookupDeveloperIdentity_maxResults,
    lookupDeveloperIdentity_developerUserIdentifier,
    lookupDeveloperIdentity_identityId,
    lookupDeveloperIdentity_identityPoolId,

    -- * Destructuring the Response
    LookupDeveloperIdentityResponse (..),
    newLookupDeveloperIdentityResponse,

    -- * Response Lenses
    lookupDeveloperIdentityResponse_nextToken,
    lookupDeveloperIdentityResponse_developerUserIdentifierList,
    lookupDeveloperIdentityResponse_identityId,
    lookupDeveloperIdentityResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @LookupDeveloperIdentityInput@ action.
--
-- /See:/ 'newLookupDeveloperIdentity' smart constructor.
data LookupDeveloperIdentity = LookupDeveloperIdentity'
  { -- | A pagination token. The first call you make will have @NextToken@ set to
    -- null. After that the service will return @NextToken@ values as needed.
    -- For example, let\'s say you make a request with @MaxResults@ set to 10,
    -- and there are 20 matches in the database. The service will return a
    -- pagination token as a part of the response. This token can be used to
    -- call the API again and get results starting from the 11th match.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of identities to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A unique ID used by your backend authentication process to identify a
    -- user. Typically, a developer identity provider would issue many
    -- developer user identifiers, in keeping with the number of users.
    developerUserIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LookupDeveloperIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'lookupDeveloperIdentity_nextToken' - A pagination token. The first call you make will have @NextToken@ set to
-- null. After that the service will return @NextToken@ values as needed.
-- For example, let\'s say you make a request with @MaxResults@ set to 10,
-- and there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
--
-- 'maxResults', 'lookupDeveloperIdentity_maxResults' - The maximum number of identities to return.
--
-- 'developerUserIdentifier', 'lookupDeveloperIdentity_developerUserIdentifier' - A unique ID used by your backend authentication process to identify a
-- user. Typically, a developer identity provider would issue many
-- developer user identifiers, in keeping with the number of users.
--
-- 'identityId', 'lookupDeveloperIdentity_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'identityPoolId', 'lookupDeveloperIdentity_identityPoolId' - An identity pool ID in the format REGION:GUID.
newLookupDeveloperIdentity ::
  -- | 'identityPoolId'
  Prelude.Text ->
  LookupDeveloperIdentity
newLookupDeveloperIdentity pIdentityPoolId_ =
  LookupDeveloperIdentity'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      developerUserIdentifier = Prelude.Nothing,
      identityId = Prelude.Nothing,
      identityPoolId = pIdentityPoolId_
    }

-- | A pagination token. The first call you make will have @NextToken@ set to
-- null. After that the service will return @NextToken@ values as needed.
-- For example, let\'s say you make a request with @MaxResults@ set to 10,
-- and there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
lookupDeveloperIdentity_nextToken :: Lens.Lens' LookupDeveloperIdentity (Prelude.Maybe Prelude.Text)
lookupDeveloperIdentity_nextToken = Lens.lens (\LookupDeveloperIdentity' {nextToken} -> nextToken) (\s@LookupDeveloperIdentity' {} a -> s {nextToken = a} :: LookupDeveloperIdentity)

-- | The maximum number of identities to return.
lookupDeveloperIdentity_maxResults :: Lens.Lens' LookupDeveloperIdentity (Prelude.Maybe Prelude.Natural)
lookupDeveloperIdentity_maxResults = Lens.lens (\LookupDeveloperIdentity' {maxResults} -> maxResults) (\s@LookupDeveloperIdentity' {} a -> s {maxResults = a} :: LookupDeveloperIdentity)

-- | A unique ID used by your backend authentication process to identify a
-- user. Typically, a developer identity provider would issue many
-- developer user identifiers, in keeping with the number of users.
lookupDeveloperIdentity_developerUserIdentifier :: Lens.Lens' LookupDeveloperIdentity (Prelude.Maybe Prelude.Text)
lookupDeveloperIdentity_developerUserIdentifier = Lens.lens (\LookupDeveloperIdentity' {developerUserIdentifier} -> developerUserIdentifier) (\s@LookupDeveloperIdentity' {} a -> s {developerUserIdentifier = a} :: LookupDeveloperIdentity)

-- | A unique identifier in the format REGION:GUID.
lookupDeveloperIdentity_identityId :: Lens.Lens' LookupDeveloperIdentity (Prelude.Maybe Prelude.Text)
lookupDeveloperIdentity_identityId = Lens.lens (\LookupDeveloperIdentity' {identityId} -> identityId) (\s@LookupDeveloperIdentity' {} a -> s {identityId = a} :: LookupDeveloperIdentity)

-- | An identity pool ID in the format REGION:GUID.
lookupDeveloperIdentity_identityPoolId :: Lens.Lens' LookupDeveloperIdentity Prelude.Text
lookupDeveloperIdentity_identityPoolId = Lens.lens (\LookupDeveloperIdentity' {identityPoolId} -> identityPoolId) (\s@LookupDeveloperIdentity' {} a -> s {identityPoolId = a} :: LookupDeveloperIdentity)

instance Core.AWSRequest LookupDeveloperIdentity where
  type
    AWSResponse LookupDeveloperIdentity =
      LookupDeveloperIdentityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          LookupDeveloperIdentityResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "DeveloperUserIdentifierList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "IdentityId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LookupDeveloperIdentity

instance Prelude.NFData LookupDeveloperIdentity

instance Core.ToHeaders LookupDeveloperIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.LookupDeveloperIdentity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON LookupDeveloperIdentity where
  toJSON LookupDeveloperIdentity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("DeveloperUserIdentifier" Core..=)
              Prelude.<$> developerUserIdentifier,
            ("IdentityId" Core..=) Prelude.<$> identityId,
            Prelude.Just
              ("IdentityPoolId" Core..= identityPoolId)
          ]
      )

instance Core.ToPath LookupDeveloperIdentity where
  toPath = Prelude.const "/"

instance Core.ToQuery LookupDeveloperIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | Returned in response to a successful @LookupDeveloperIdentity@ action.
--
-- /See:/ 'newLookupDeveloperIdentityResponse' smart constructor.
data LookupDeveloperIdentityResponse = LookupDeveloperIdentityResponse'
  { -- | A pagination token. The first call you make will have @NextToken@ set to
    -- null. After that the service will return @NextToken@ values as needed.
    -- For example, let\'s say you make a request with @MaxResults@ set to 10,
    -- and there are 20 matches in the database. The service will return a
    -- pagination token as a part of the response. This token can be used to
    -- call the API again and get results starting from the 11th match.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This is the list of developer user identifiers associated with an
    -- identity ID. Cognito supports the association of multiple developer user
    -- identifiers with an identity ID.
    developerUserIdentifierList :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LookupDeveloperIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'lookupDeveloperIdentityResponse_nextToken' - A pagination token. The first call you make will have @NextToken@ set to
-- null. After that the service will return @NextToken@ values as needed.
-- For example, let\'s say you make a request with @MaxResults@ set to 10,
-- and there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
--
-- 'developerUserIdentifierList', 'lookupDeveloperIdentityResponse_developerUserIdentifierList' - This is the list of developer user identifiers associated with an
-- identity ID. Cognito supports the association of multiple developer user
-- identifiers with an identity ID.
--
-- 'identityId', 'lookupDeveloperIdentityResponse_identityId' - A unique identifier in the format REGION:GUID.
--
-- 'httpStatus', 'lookupDeveloperIdentityResponse_httpStatus' - The response's http status code.
newLookupDeveloperIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  LookupDeveloperIdentityResponse
newLookupDeveloperIdentityResponse pHttpStatus_ =
  LookupDeveloperIdentityResponse'
    { nextToken =
        Prelude.Nothing,
      developerUserIdentifierList =
        Prelude.Nothing,
      identityId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token. The first call you make will have @NextToken@ set to
-- null. After that the service will return @NextToken@ values as needed.
-- For example, let\'s say you make a request with @MaxResults@ set to 10,
-- and there are 20 matches in the database. The service will return a
-- pagination token as a part of the response. This token can be used to
-- call the API again and get results starting from the 11th match.
lookupDeveloperIdentityResponse_nextToken :: Lens.Lens' LookupDeveloperIdentityResponse (Prelude.Maybe Prelude.Text)
lookupDeveloperIdentityResponse_nextToken = Lens.lens (\LookupDeveloperIdentityResponse' {nextToken} -> nextToken) (\s@LookupDeveloperIdentityResponse' {} a -> s {nextToken = a} :: LookupDeveloperIdentityResponse)

-- | This is the list of developer user identifiers associated with an
-- identity ID. Cognito supports the association of multiple developer user
-- identifiers with an identity ID.
lookupDeveloperIdentityResponse_developerUserIdentifierList :: Lens.Lens' LookupDeveloperIdentityResponse (Prelude.Maybe [Prelude.Text])
lookupDeveloperIdentityResponse_developerUserIdentifierList = Lens.lens (\LookupDeveloperIdentityResponse' {developerUserIdentifierList} -> developerUserIdentifierList) (\s@LookupDeveloperIdentityResponse' {} a -> s {developerUserIdentifierList = a} :: LookupDeveloperIdentityResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier in the format REGION:GUID.
lookupDeveloperIdentityResponse_identityId :: Lens.Lens' LookupDeveloperIdentityResponse (Prelude.Maybe Prelude.Text)
lookupDeveloperIdentityResponse_identityId = Lens.lens (\LookupDeveloperIdentityResponse' {identityId} -> identityId) (\s@LookupDeveloperIdentityResponse' {} a -> s {identityId = a} :: LookupDeveloperIdentityResponse)

-- | The response's http status code.
lookupDeveloperIdentityResponse_httpStatus :: Lens.Lens' LookupDeveloperIdentityResponse Prelude.Int
lookupDeveloperIdentityResponse_httpStatus = Lens.lens (\LookupDeveloperIdentityResponse' {httpStatus} -> httpStatus) (\s@LookupDeveloperIdentityResponse' {} a -> s {httpStatus = a} :: LookupDeveloperIdentityResponse)

instance
  Prelude.NFData
    LookupDeveloperIdentityResponse
