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
-- Module      : Amazonka.CustomerProfiles.GetProfileObjectType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the object types for a specific domain.
module Amazonka.CustomerProfiles.GetProfileObjectType
  ( -- * Creating a Request
    GetProfileObjectType (..),
    newGetProfileObjectType,

    -- * Request Lenses
    getProfileObjectType_domainName,
    getProfileObjectType_objectTypeName,

    -- * Destructuring the Response
    GetProfileObjectTypeResponse (..),
    newGetProfileObjectTypeResponse,

    -- * Response Lenses
    getProfileObjectTypeResponse_expirationDays,
    getProfileObjectTypeResponse_lastUpdatedAt,
    getProfileObjectTypeResponse_createdAt,
    getProfileObjectTypeResponse_templateId,
    getProfileObjectTypeResponse_keys,
    getProfileObjectTypeResponse_encryptionKey,
    getProfileObjectTypeResponse_allowProfileCreation,
    getProfileObjectTypeResponse_fields,
    getProfileObjectTypeResponse_tags,
    getProfileObjectTypeResponse_httpStatus,
    getProfileObjectTypeResponse_objectTypeName,
    getProfileObjectTypeResponse_description,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProfileObjectType' smart constructor.
data GetProfileObjectType = GetProfileObjectType'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfileObjectType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getProfileObjectType_domainName' - The unique name of the domain.
--
-- 'objectTypeName', 'getProfileObjectType_objectTypeName' - The name of the profile object type.
newGetProfileObjectType ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'objectTypeName'
  Prelude.Text ->
  GetProfileObjectType
newGetProfileObjectType pDomainName_ pObjectTypeName_ =
  GetProfileObjectType'
    { domainName = pDomainName_,
      objectTypeName = pObjectTypeName_
    }

-- | The unique name of the domain.
getProfileObjectType_domainName :: Lens.Lens' GetProfileObjectType Prelude.Text
getProfileObjectType_domainName = Lens.lens (\GetProfileObjectType' {domainName} -> domainName) (\s@GetProfileObjectType' {} a -> s {domainName = a} :: GetProfileObjectType)

-- | The name of the profile object type.
getProfileObjectType_objectTypeName :: Lens.Lens' GetProfileObjectType Prelude.Text
getProfileObjectType_objectTypeName = Lens.lens (\GetProfileObjectType' {objectTypeName} -> objectTypeName) (\s@GetProfileObjectType' {} a -> s {objectTypeName = a} :: GetProfileObjectType)

instance Core.AWSRequest GetProfileObjectType where
  type
    AWSResponse GetProfileObjectType =
      GetProfileObjectTypeResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProfileObjectTypeResponse'
            Prelude.<$> (x Core..?> "ExpirationDays")
            Prelude.<*> (x Core..?> "LastUpdatedAt")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "TemplateId")
            Prelude.<*> (x Core..?> "Keys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "EncryptionKey")
            Prelude.<*> (x Core..?> "AllowProfileCreation")
            Prelude.<*> (x Core..?> "Fields" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ObjectTypeName")
            Prelude.<*> (x Core..:> "Description")
      )

instance Prelude.Hashable GetProfileObjectType where
  hashWithSalt _salt GetProfileObjectType' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` objectTypeName

instance Prelude.NFData GetProfileObjectType where
  rnf GetProfileObjectType' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf objectTypeName

instance Core.ToHeaders GetProfileObjectType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetProfileObjectType where
  toPath GetProfileObjectType' {..} =
    Prelude.mconcat
      [ "/domains/",
        Core.toBS domainName,
        "/object-types/",
        Core.toBS objectTypeName
      ]

instance Core.ToQuery GetProfileObjectType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProfileObjectTypeResponse' smart constructor.
data GetProfileObjectTypeResponse = GetProfileObjectTypeResponse'
  { -- | The number of days until the data in the object expires.
    expirationDays :: Prelude.Maybe Prelude.Natural,
    -- | The timestamp of when the domain was most recently edited.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The timestamp of when the domain was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | A unique identifier for the object template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | A list of unique keys that can be used to map data to the profile.
    keys :: Prelude.Maybe (Prelude.HashMap Prelude.Text [ObjectTypeKey]),
    -- | The customer-provided key to encrypt the profile object that will be
    -- created in this profile object type.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a profile should be created when data is received if
    -- one doesn’t exist for an object of this type. The default is @FALSE@. If
    -- the AllowProfileCreation flag is set to @FALSE@, then the service tries
    -- to fetch a standard profile and associate this object with the profile.
    -- If it is set to @TRUE@, and if no match is found, then the service
    -- creates a new standard profile.
    allowProfileCreation :: Prelude.Maybe Prelude.Bool,
    -- | A map of the name and ObjectType field.
    fields :: Prelude.Maybe (Prelude.HashMap Prelude.Text ObjectTypeField),
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text,
    -- | The description of the profile object type.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfileObjectTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationDays', 'getProfileObjectTypeResponse_expirationDays' - The number of days until the data in the object expires.
--
-- 'lastUpdatedAt', 'getProfileObjectTypeResponse_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
--
-- 'createdAt', 'getProfileObjectTypeResponse_createdAt' - The timestamp of when the domain was created.
--
-- 'templateId', 'getProfileObjectTypeResponse_templateId' - A unique identifier for the object template.
--
-- 'keys', 'getProfileObjectTypeResponse_keys' - A list of unique keys that can be used to map data to the profile.
--
-- 'encryptionKey', 'getProfileObjectTypeResponse_encryptionKey' - The customer-provided key to encrypt the profile object that will be
-- created in this profile object type.
--
-- 'allowProfileCreation', 'getProfileObjectTypeResponse_allowProfileCreation' - Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
--
-- 'fields', 'getProfileObjectTypeResponse_fields' - A map of the name and ObjectType field.
--
-- 'tags', 'getProfileObjectTypeResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'getProfileObjectTypeResponse_httpStatus' - The response's http status code.
--
-- 'objectTypeName', 'getProfileObjectTypeResponse_objectTypeName' - The name of the profile object type.
--
-- 'description', 'getProfileObjectTypeResponse_description' - The description of the profile object type.
newGetProfileObjectTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  GetProfileObjectTypeResponse
newGetProfileObjectTypeResponse
  pHttpStatus_
  pObjectTypeName_
  pDescription_ =
    GetProfileObjectTypeResponse'
      { expirationDays =
          Prelude.Nothing,
        lastUpdatedAt = Prelude.Nothing,
        createdAt = Prelude.Nothing,
        templateId = Prelude.Nothing,
        keys = Prelude.Nothing,
        encryptionKey = Prelude.Nothing,
        allowProfileCreation = Prelude.Nothing,
        fields = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        objectTypeName = pObjectTypeName_,
        description = pDescription_
      }

-- | The number of days until the data in the object expires.
getProfileObjectTypeResponse_expirationDays :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Natural)
getProfileObjectTypeResponse_expirationDays = Lens.lens (\GetProfileObjectTypeResponse' {expirationDays} -> expirationDays) (\s@GetProfileObjectTypeResponse' {} a -> s {expirationDays = a} :: GetProfileObjectTypeResponse)

-- | The timestamp of when the domain was most recently edited.
getProfileObjectTypeResponse_lastUpdatedAt :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.UTCTime)
getProfileObjectTypeResponse_lastUpdatedAt = Lens.lens (\GetProfileObjectTypeResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetProfileObjectTypeResponse' {} a -> s {lastUpdatedAt = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The timestamp of when the domain was created.
getProfileObjectTypeResponse_createdAt :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.UTCTime)
getProfileObjectTypeResponse_createdAt = Lens.lens (\GetProfileObjectTypeResponse' {createdAt} -> createdAt) (\s@GetProfileObjectTypeResponse' {} a -> s {createdAt = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Core._Time

-- | A unique identifier for the object template.
getProfileObjectTypeResponse_templateId :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeResponse_templateId = Lens.lens (\GetProfileObjectTypeResponse' {templateId} -> templateId) (\s@GetProfileObjectTypeResponse' {} a -> s {templateId = a} :: GetProfileObjectTypeResponse)

-- | A list of unique keys that can be used to map data to the profile.
getProfileObjectTypeResponse_keys :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [ObjectTypeKey]))
getProfileObjectTypeResponse_keys = Lens.lens (\GetProfileObjectTypeResponse' {keys} -> keys) (\s@GetProfileObjectTypeResponse' {} a -> s {keys = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The customer-provided key to encrypt the profile object that will be
-- created in this profile object type.
getProfileObjectTypeResponse_encryptionKey :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeResponse_encryptionKey = Lens.lens (\GetProfileObjectTypeResponse' {encryptionKey} -> encryptionKey) (\s@GetProfileObjectTypeResponse' {} a -> s {encryptionKey = a} :: GetProfileObjectTypeResponse)

-- | Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
getProfileObjectTypeResponse_allowProfileCreation :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Bool)
getProfileObjectTypeResponse_allowProfileCreation = Lens.lens (\GetProfileObjectTypeResponse' {allowProfileCreation} -> allowProfileCreation) (\s@GetProfileObjectTypeResponse' {} a -> s {allowProfileCreation = a} :: GetProfileObjectTypeResponse)

-- | A map of the name and ObjectType field.
getProfileObjectTypeResponse_fields :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ObjectTypeField))
getProfileObjectTypeResponse_fields = Lens.lens (\GetProfileObjectTypeResponse' {fields} -> fields) (\s@GetProfileObjectTypeResponse' {} a -> s {fields = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
getProfileObjectTypeResponse_tags :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getProfileObjectTypeResponse_tags = Lens.lens (\GetProfileObjectTypeResponse' {tags} -> tags) (\s@GetProfileObjectTypeResponse' {} a -> s {tags = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getProfileObjectTypeResponse_httpStatus :: Lens.Lens' GetProfileObjectTypeResponse Prelude.Int
getProfileObjectTypeResponse_httpStatus = Lens.lens (\GetProfileObjectTypeResponse' {httpStatus} -> httpStatus) (\s@GetProfileObjectTypeResponse' {} a -> s {httpStatus = a} :: GetProfileObjectTypeResponse)

-- | The name of the profile object type.
getProfileObjectTypeResponse_objectTypeName :: Lens.Lens' GetProfileObjectTypeResponse Prelude.Text
getProfileObjectTypeResponse_objectTypeName = Lens.lens (\GetProfileObjectTypeResponse' {objectTypeName} -> objectTypeName) (\s@GetProfileObjectTypeResponse' {} a -> s {objectTypeName = a} :: GetProfileObjectTypeResponse)

-- | The description of the profile object type.
getProfileObjectTypeResponse_description :: Lens.Lens' GetProfileObjectTypeResponse Prelude.Text
getProfileObjectTypeResponse_description = Lens.lens (\GetProfileObjectTypeResponse' {description} -> description) (\s@GetProfileObjectTypeResponse' {} a -> s {description = a} :: GetProfileObjectTypeResponse)

instance Prelude.NFData GetProfileObjectTypeResponse where
  rnf GetProfileObjectTypeResponse' {..} =
    Prelude.rnf expirationDays
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf keys
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf allowProfileCreation
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf description
