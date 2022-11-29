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
-- Module      : Amazonka.EMR.GetStudioSessionMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches mapping details for the specified Amazon EMR Studio and identity
-- (user or group).
module Amazonka.EMR.GetStudioSessionMapping
  ( -- * Creating a Request
    GetStudioSessionMapping (..),
    newGetStudioSessionMapping,

    -- * Request Lenses
    getStudioSessionMapping_identityName,
    getStudioSessionMapping_identityId,
    getStudioSessionMapping_studioId,
    getStudioSessionMapping_identityType,

    -- * Destructuring the Response
    GetStudioSessionMappingResponse (..),
    newGetStudioSessionMappingResponse,

    -- * Response Lenses
    getStudioSessionMappingResponse_sessionMapping,
    getStudioSessionMappingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStudioSessionMapping' smart constructor.
data GetStudioSessionMapping = GetStudioSessionMapping'
  { -- | The name of the user or group to fetch. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /Amazon Web Services SSO Identity Store API Reference/. Either
    -- @IdentityName@ or @IdentityId@ must be specified.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The globally unique identifier (GUID) of the user or group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /Amazon Web Services SSO Identity Store API Reference/. Either
    -- @IdentityName@ or @IdentityId@ must be specified.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Text,
    -- | Specifies whether the identity to fetch is a user or a group.
    identityType :: IdentityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStudioSessionMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityName', 'getStudioSessionMapping_identityName' - The name of the user or group to fetch. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /Amazon Web Services SSO Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
--
-- 'identityId', 'getStudioSessionMapping_identityId' - The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /Amazon Web Services SSO Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
--
-- 'studioId', 'getStudioSessionMapping_studioId' - The ID of the Amazon EMR Studio.
--
-- 'identityType', 'getStudioSessionMapping_identityType' - Specifies whether the identity to fetch is a user or a group.
newGetStudioSessionMapping ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'identityType'
  IdentityType ->
  GetStudioSessionMapping
newGetStudioSessionMapping pStudioId_ pIdentityType_ =
  GetStudioSessionMapping'
    { identityName =
        Prelude.Nothing,
      identityId = Prelude.Nothing,
      studioId = pStudioId_,
      identityType = pIdentityType_
    }

-- | The name of the user or group to fetch. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /Amazon Web Services SSO Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
getStudioSessionMapping_identityName :: Lens.Lens' GetStudioSessionMapping (Prelude.Maybe Prelude.Text)
getStudioSessionMapping_identityName = Lens.lens (\GetStudioSessionMapping' {identityName} -> identityName) (\s@GetStudioSessionMapping' {} a -> s {identityName = a} :: GetStudioSessionMapping)

-- | The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /Amazon Web Services SSO Identity Store API Reference/. Either
-- @IdentityName@ or @IdentityId@ must be specified.
getStudioSessionMapping_identityId :: Lens.Lens' GetStudioSessionMapping (Prelude.Maybe Prelude.Text)
getStudioSessionMapping_identityId = Lens.lens (\GetStudioSessionMapping' {identityId} -> identityId) (\s@GetStudioSessionMapping' {} a -> s {identityId = a} :: GetStudioSessionMapping)

-- | The ID of the Amazon EMR Studio.
getStudioSessionMapping_studioId :: Lens.Lens' GetStudioSessionMapping Prelude.Text
getStudioSessionMapping_studioId = Lens.lens (\GetStudioSessionMapping' {studioId} -> studioId) (\s@GetStudioSessionMapping' {} a -> s {studioId = a} :: GetStudioSessionMapping)

-- | Specifies whether the identity to fetch is a user or a group.
getStudioSessionMapping_identityType :: Lens.Lens' GetStudioSessionMapping IdentityType
getStudioSessionMapping_identityType = Lens.lens (\GetStudioSessionMapping' {identityType} -> identityType) (\s@GetStudioSessionMapping' {} a -> s {identityType = a} :: GetStudioSessionMapping)

instance Core.AWSRequest GetStudioSessionMapping where
  type
    AWSResponse GetStudioSessionMapping =
      GetStudioSessionMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStudioSessionMappingResponse'
            Prelude.<$> (x Core..?> "SessionMapping")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStudioSessionMapping where
  hashWithSalt _salt GetStudioSessionMapping' {..} =
    _salt `Prelude.hashWithSalt` identityName
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` identityType

instance Prelude.NFData GetStudioSessionMapping where
  rnf GetStudioSessionMapping' {..} =
    Prelude.rnf identityName
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf identityType

instance Core.ToHeaders GetStudioSessionMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.GetStudioSessionMapping" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetStudioSessionMapping where
  toJSON GetStudioSessionMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IdentityName" Core..=) Prelude.<$> identityName,
            ("IdentityId" Core..=) Prelude.<$> identityId,
            Prelude.Just ("StudioId" Core..= studioId),
            Prelude.Just ("IdentityType" Core..= identityType)
          ]
      )

instance Core.ToPath GetStudioSessionMapping where
  toPath = Prelude.const "/"

instance Core.ToQuery GetStudioSessionMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStudioSessionMappingResponse' smart constructor.
data GetStudioSessionMappingResponse = GetStudioSessionMappingResponse'
  { -- | The session mapping details for the specified Amazon EMR Studio and
    -- identity, including session policy ARN and creation time.
    sessionMapping :: Prelude.Maybe SessionMappingDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStudioSessionMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sessionMapping', 'getStudioSessionMappingResponse_sessionMapping' - The session mapping details for the specified Amazon EMR Studio and
-- identity, including session policy ARN and creation time.
--
-- 'httpStatus', 'getStudioSessionMappingResponse_httpStatus' - The response's http status code.
newGetStudioSessionMappingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStudioSessionMappingResponse
newGetStudioSessionMappingResponse pHttpStatus_ =
  GetStudioSessionMappingResponse'
    { sessionMapping =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session mapping details for the specified Amazon EMR Studio and
-- identity, including session policy ARN and creation time.
getStudioSessionMappingResponse_sessionMapping :: Lens.Lens' GetStudioSessionMappingResponse (Prelude.Maybe SessionMappingDetail)
getStudioSessionMappingResponse_sessionMapping = Lens.lens (\GetStudioSessionMappingResponse' {sessionMapping} -> sessionMapping) (\s@GetStudioSessionMappingResponse' {} a -> s {sessionMapping = a} :: GetStudioSessionMappingResponse)

-- | The response's http status code.
getStudioSessionMappingResponse_httpStatus :: Lens.Lens' GetStudioSessionMappingResponse Prelude.Int
getStudioSessionMappingResponse_httpStatus = Lens.lens (\GetStudioSessionMappingResponse' {httpStatus} -> httpStatus) (\s@GetStudioSessionMappingResponse' {} a -> s {httpStatus = a} :: GetStudioSessionMappingResponse)

instance
  Prelude.NFData
    GetStudioSessionMappingResponse
  where
  rnf GetStudioSessionMappingResponse' {..} =
    Prelude.rnf sessionMapping
      `Prelude.seq` Prelude.rnf httpStatus
