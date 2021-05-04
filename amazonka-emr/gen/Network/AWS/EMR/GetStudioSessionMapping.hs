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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The globally unique identifier (GUID) of the user or group. For more
    -- information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
    -- and
    -- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
    -- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
    -- @IdentityId@ must be specified.
    identityId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Prelude.Text,
    -- | Specifies whether the identity to fetch is a user or a group.
    identityType :: IdentityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
getStudioSessionMapping_identityName :: Lens.Lens' GetStudioSessionMapping (Prelude.Maybe Prelude.Text)
getStudioSessionMapping_identityName = Lens.lens (\GetStudioSessionMapping' {identityName} -> identityName) (\s@GetStudioSessionMapping' {} a -> s {identityName = a} :: GetStudioSessionMapping)

-- | The globally unique identifier (GUID) of the user or group. For more
-- information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserId>
-- and
-- <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-GroupId GroupId>
-- in the /AWS SSO Identity Store API Reference/. Either @IdentityName@ or
-- @IdentityId@ must be specified.
getStudioSessionMapping_identityId :: Lens.Lens' GetStudioSessionMapping (Prelude.Maybe Prelude.Text)
getStudioSessionMapping_identityId = Lens.lens (\GetStudioSessionMapping' {identityId} -> identityId) (\s@GetStudioSessionMapping' {} a -> s {identityId = a} :: GetStudioSessionMapping)

-- | The ID of the Amazon EMR Studio.
getStudioSessionMapping_studioId :: Lens.Lens' GetStudioSessionMapping Prelude.Text
getStudioSessionMapping_studioId = Lens.lens (\GetStudioSessionMapping' {studioId} -> studioId) (\s@GetStudioSessionMapping' {} a -> s {studioId = a} :: GetStudioSessionMapping)

-- | Specifies whether the identity to fetch is a user or a group.
getStudioSessionMapping_identityType :: Lens.Lens' GetStudioSessionMapping IdentityType
getStudioSessionMapping_identityType = Lens.lens (\GetStudioSessionMapping' {identityType} -> identityType) (\s@GetStudioSessionMapping' {} a -> s {identityType = a} :: GetStudioSessionMapping)

instance Prelude.AWSRequest GetStudioSessionMapping where
  type
    Rs GetStudioSessionMapping =
      GetStudioSessionMappingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStudioSessionMappingResponse'
            Prelude.<$> (x Prelude..?> "SessionMapping")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStudioSessionMapping

instance Prelude.NFData GetStudioSessionMapping

instance Prelude.ToHeaders GetStudioSessionMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.GetStudioSessionMapping" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetStudioSessionMapping where
  toJSON GetStudioSessionMapping' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IdentityName" Prelude..=)
              Prelude.<$> identityName,
            ("IdentityId" Prelude..=) Prelude.<$> identityId,
            Prelude.Just ("StudioId" Prelude..= studioId),
            Prelude.Just
              ("IdentityType" Prelude..= identityType)
          ]
      )

instance Prelude.ToPath GetStudioSessionMapping where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetStudioSessionMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStudioSessionMappingResponse' smart constructor.
data GetStudioSessionMappingResponse = GetStudioSessionMappingResponse'
  { -- | The session mapping details for the specified Amazon EMR Studio and
    -- identity, including session policy ARN and creation time.
    sessionMapping :: Prelude.Maybe SessionMappingDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
