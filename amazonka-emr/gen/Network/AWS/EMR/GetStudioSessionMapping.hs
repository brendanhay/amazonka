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
-- Module      : Network.AWS.EMR.GetStudioSessionMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches mapping details for the specified Amazon EMR Studio and identity
-- (user or group).
module Network.AWS.EMR.GetStudioSessionMapping
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

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetStudioSessionMapping' smart constructor.
data GetStudioSessionMapping = GetStudioSessionMapping'
  { -- | The name of the user or group to fetch. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityName :: Core.Maybe Core.Text,
    -- | The globally unique identifier (GUID) of the user or group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityId :: Core.Maybe Core.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Core.Text,
    -- | Specifies whether the identity to fetch is a user or a group.
    identityType :: IdentityType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
--
-- 'identityId', 'getStudioSessionMapping_identityId' - The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
--
-- 'studioId', 'getStudioSessionMapping_studioId' - The ID of the Amazon EMR Studio.
--
-- 'identityType', 'getStudioSessionMapping_identityType' - Specifies whether the identity to fetch is a user or a group.
newGetStudioSessionMapping ::
  -- | 'studioId'
  Core.Text ->
  -- | 'identityType'
  IdentityType ->
  GetStudioSessionMapping
newGetStudioSessionMapping pStudioId_ pIdentityType_ =
  GetStudioSessionMapping'
    { identityName =
        Core.Nothing,
      identityId = Core.Nothing,
      studioId = pStudioId_,
      identityType = pIdentityType_
    }

-- | The name of the user or group to fetch. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserName UserName>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
getStudioSessionMapping_identityName :: Lens.Lens' GetStudioSessionMapping (Core.Maybe Core.Text)
getStudioSessionMapping_identityName = Lens.lens (\GetStudioSessionMapping' {identityName} -> identityName) (\s@GetStudioSessionMapping' {} a -> s {identityName = a} :: GetStudioSessionMapping)

-- | The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
getStudioSessionMapping_identityId :: Lens.Lens' GetStudioSessionMapping (Core.Maybe Core.Text)
getStudioSessionMapping_identityId = Lens.lens (\GetStudioSessionMapping' {identityId} -> identityId) (\s@GetStudioSessionMapping' {} a -> s {identityId = a} :: GetStudioSessionMapping)

-- | The ID of the Amazon EMR Studio.
getStudioSessionMapping_studioId :: Lens.Lens' GetStudioSessionMapping Core.Text
getStudioSessionMapping_studioId = Lens.lens (\GetStudioSessionMapping' {studioId} -> studioId) (\s@GetStudioSessionMapping' {} a -> s {studioId = a} :: GetStudioSessionMapping)

-- | Specifies whether the identity to fetch is a user or a group.
getStudioSessionMapping_identityType :: Lens.Lens' GetStudioSessionMapping IdentityType
getStudioSessionMapping_identityType = Lens.lens (\GetStudioSessionMapping' {identityType} -> identityType) (\s@GetStudioSessionMapping' {} a -> s {identityType = a} :: GetStudioSessionMapping)

instance Core.AWSRequest GetStudioSessionMapping where
  type
    AWSResponse GetStudioSessionMapping =
      GetStudioSessionMappingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStudioSessionMappingResponse'
            Core.<$> (x Core..?> "SessionMapping")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetStudioSessionMapping

instance Core.NFData GetStudioSessionMapping

instance Core.ToHeaders GetStudioSessionMapping where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.GetStudioSessionMapping" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetStudioSessionMapping where
  toJSON GetStudioSessionMapping' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IdentityName" Core..=) Core.<$> identityName,
            ("IdentityId" Core..=) Core.<$> identityId,
            Core.Just ("StudioId" Core..= studioId),
            Core.Just ("IdentityType" Core..= identityType)
          ]
      )

instance Core.ToPath GetStudioSessionMapping where
  toPath = Core.const "/"

instance Core.ToQuery GetStudioSessionMapping where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetStudioSessionMappingResponse' smart constructor.
data GetStudioSessionMappingResponse = GetStudioSessionMappingResponse'
  { -- | The session mapping details for the specified Amazon EMR Studio and
    -- identity, including session policy ARN and creation time.
    sessionMapping :: Core.Maybe SessionMappingDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetStudioSessionMappingResponse
newGetStudioSessionMappingResponse pHttpStatus_ =
  GetStudioSessionMappingResponse'
    { sessionMapping =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session mapping details for the specified Amazon EMR Studio and
-- identity, including session policy ARN and creation time.
getStudioSessionMappingResponse_sessionMapping :: Lens.Lens' GetStudioSessionMappingResponse (Core.Maybe SessionMappingDetail)
getStudioSessionMappingResponse_sessionMapping = Lens.lens (\GetStudioSessionMappingResponse' {sessionMapping} -> sessionMapping) (\s@GetStudioSessionMappingResponse' {} a -> s {sessionMapping = a} :: GetStudioSessionMappingResponse)

-- | The response's http status code.
getStudioSessionMappingResponse_httpStatus :: Lens.Lens' GetStudioSessionMappingResponse Core.Int
getStudioSessionMappingResponse_httpStatus = Lens.lens (\GetStudioSessionMappingResponse' {httpStatus} -> httpStatus) (\s@GetStudioSessionMappingResponse' {} a -> s {httpStatus = a} :: GetStudioSessionMappingResponse)

instance Core.NFData GetStudioSessionMappingResponse
