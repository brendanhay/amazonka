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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    getProfileObjectTypeResponse_allowProfileCreation,
    getProfileObjectTypeResponse_createdAt,
    getProfileObjectTypeResponse_encryptionKey,
    getProfileObjectTypeResponse_expirationDays,
    getProfileObjectTypeResponse_fields,
    getProfileObjectTypeResponse_keys,
    getProfileObjectTypeResponse_lastUpdatedAt,
    getProfileObjectTypeResponse_sourceLastUpdatedTimestampFormat,
    getProfileObjectTypeResponse_tags,
    getProfileObjectTypeResponse_templateId,
    getProfileObjectTypeResponse_httpStatus,
    getProfileObjectTypeResponse_objectTypeName,
    getProfileObjectTypeResponse_description,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProfileObjectTypeResponse'
            Prelude.<$> (x Data..?> "AllowProfileCreation")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "EncryptionKey")
            Prelude.<*> (x Data..?> "ExpirationDays")
            Prelude.<*> (x Data..?> "Fields" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Keys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "LastUpdatedAt")
            Prelude.<*> (x Data..?> "SourceLastUpdatedTimestampFormat")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TemplateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ObjectTypeName")
            Prelude.<*> (x Data..:> "Description")
      )

instance Prelude.Hashable GetProfileObjectType where
  hashWithSalt _salt GetProfileObjectType' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` objectTypeName

instance Prelude.NFData GetProfileObjectType where
  rnf GetProfileObjectType' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf objectTypeName

instance Data.ToHeaders GetProfileObjectType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetProfileObjectType where
  toPath GetProfileObjectType' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/object-types/",
        Data.toBS objectTypeName
      ]

instance Data.ToQuery GetProfileObjectType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProfileObjectTypeResponse' smart constructor.
data GetProfileObjectTypeResponse = GetProfileObjectTypeResponse'
  { -- | Indicates whether a profile should be created when data is received if
    -- one doesn’t exist for an object of this type. The default is @FALSE@. If
    -- the AllowProfileCreation flag is set to @FALSE@, then the service tries
    -- to fetch a standard profile and associate this object with the profile.
    -- If it is set to @TRUE@, and if no match is found, then the service
    -- creates a new standard profile.
    allowProfileCreation :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp of when the domain was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The customer-provided key to encrypt the profile object that will be
    -- created in this profile object type.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The number of days until the data in the object expires.
    expirationDays :: Prelude.Maybe Prelude.Natural,
    -- | A map of the name and ObjectType field.
    fields :: Prelude.Maybe (Prelude.HashMap Prelude.Text ObjectTypeField),
    -- | A list of unique keys that can be used to map data to the profile.
    keys :: Prelude.Maybe (Prelude.HashMap Prelude.Text [ObjectTypeKey]),
    -- | The timestamp of when the domain was most recently edited.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The format of your @sourceLastUpdatedTimestamp@ that was previously set
    -- up.
    sourceLastUpdatedTimestampFormat :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique identifier for the object template.
    templateId :: Prelude.Maybe Prelude.Text,
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
-- 'allowProfileCreation', 'getProfileObjectTypeResponse_allowProfileCreation' - Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
--
-- 'createdAt', 'getProfileObjectTypeResponse_createdAt' - The timestamp of when the domain was created.
--
-- 'encryptionKey', 'getProfileObjectTypeResponse_encryptionKey' - The customer-provided key to encrypt the profile object that will be
-- created in this profile object type.
--
-- 'expirationDays', 'getProfileObjectTypeResponse_expirationDays' - The number of days until the data in the object expires.
--
-- 'fields', 'getProfileObjectTypeResponse_fields' - A map of the name and ObjectType field.
--
-- 'keys', 'getProfileObjectTypeResponse_keys' - A list of unique keys that can be used to map data to the profile.
--
-- 'lastUpdatedAt', 'getProfileObjectTypeResponse_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
--
-- 'sourceLastUpdatedTimestampFormat', 'getProfileObjectTypeResponse_sourceLastUpdatedTimestampFormat' - The format of your @sourceLastUpdatedTimestamp@ that was previously set
-- up.
--
-- 'tags', 'getProfileObjectTypeResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'templateId', 'getProfileObjectTypeResponse_templateId' - A unique identifier for the object template.
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
      { allowProfileCreation =
          Prelude.Nothing,
        createdAt = Prelude.Nothing,
        encryptionKey = Prelude.Nothing,
        expirationDays = Prelude.Nothing,
        fields = Prelude.Nothing,
        keys = Prelude.Nothing,
        lastUpdatedAt = Prelude.Nothing,
        sourceLastUpdatedTimestampFormat =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        templateId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        objectTypeName = pObjectTypeName_,
        description = pDescription_
      }

-- | Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
getProfileObjectTypeResponse_allowProfileCreation :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Bool)
getProfileObjectTypeResponse_allowProfileCreation = Lens.lens (\GetProfileObjectTypeResponse' {allowProfileCreation} -> allowProfileCreation) (\s@GetProfileObjectTypeResponse' {} a -> s {allowProfileCreation = a} :: GetProfileObjectTypeResponse)

-- | The timestamp of when the domain was created.
getProfileObjectTypeResponse_createdAt :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.UTCTime)
getProfileObjectTypeResponse_createdAt = Lens.lens (\GetProfileObjectTypeResponse' {createdAt} -> createdAt) (\s@GetProfileObjectTypeResponse' {} a -> s {createdAt = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Data._Time

-- | The customer-provided key to encrypt the profile object that will be
-- created in this profile object type.
getProfileObjectTypeResponse_encryptionKey :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeResponse_encryptionKey = Lens.lens (\GetProfileObjectTypeResponse' {encryptionKey} -> encryptionKey) (\s@GetProfileObjectTypeResponse' {} a -> s {encryptionKey = a} :: GetProfileObjectTypeResponse)

-- | The number of days until the data in the object expires.
getProfileObjectTypeResponse_expirationDays :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Natural)
getProfileObjectTypeResponse_expirationDays = Lens.lens (\GetProfileObjectTypeResponse' {expirationDays} -> expirationDays) (\s@GetProfileObjectTypeResponse' {} a -> s {expirationDays = a} :: GetProfileObjectTypeResponse)

-- | A map of the name and ObjectType field.
getProfileObjectTypeResponse_fields :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ObjectTypeField))
getProfileObjectTypeResponse_fields = Lens.lens (\GetProfileObjectTypeResponse' {fields} -> fields) (\s@GetProfileObjectTypeResponse' {} a -> s {fields = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of unique keys that can be used to map data to the profile.
getProfileObjectTypeResponse_keys :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [ObjectTypeKey]))
getProfileObjectTypeResponse_keys = Lens.lens (\GetProfileObjectTypeResponse' {keys} -> keys) (\s@GetProfileObjectTypeResponse' {} a -> s {keys = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of when the domain was most recently edited.
getProfileObjectTypeResponse_lastUpdatedAt :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.UTCTime)
getProfileObjectTypeResponse_lastUpdatedAt = Lens.lens (\GetProfileObjectTypeResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetProfileObjectTypeResponse' {} a -> s {lastUpdatedAt = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Data._Time

-- | The format of your @sourceLastUpdatedTimestamp@ that was previously set
-- up.
getProfileObjectTypeResponse_sourceLastUpdatedTimestampFormat :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeResponse_sourceLastUpdatedTimestampFormat = Lens.lens (\GetProfileObjectTypeResponse' {sourceLastUpdatedTimestampFormat} -> sourceLastUpdatedTimestampFormat) (\s@GetProfileObjectTypeResponse' {} a -> s {sourceLastUpdatedTimestampFormat = a} :: GetProfileObjectTypeResponse)

-- | The tags used to organize, track, or control access for this resource.
getProfileObjectTypeResponse_tags :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getProfileObjectTypeResponse_tags = Lens.lens (\GetProfileObjectTypeResponse' {tags} -> tags) (\s@GetProfileObjectTypeResponse' {} a -> s {tags = a} :: GetProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the object template.
getProfileObjectTypeResponse_templateId :: Lens.Lens' GetProfileObjectTypeResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeResponse_templateId = Lens.lens (\GetProfileObjectTypeResponse' {templateId} -> templateId) (\s@GetProfileObjectTypeResponse' {} a -> s {templateId = a} :: GetProfileObjectTypeResponse)

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
    Prelude.rnf allowProfileCreation
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf expirationDays
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf keys
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf sourceLastUpdatedTimestampFormat
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf description
