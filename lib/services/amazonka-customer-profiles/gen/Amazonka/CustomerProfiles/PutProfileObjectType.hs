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
-- Module      : Amazonka.CustomerProfiles.PutProfileObjectType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a ProfileObjectType.
module Amazonka.CustomerProfiles.PutProfileObjectType
  ( -- * Creating a Request
    PutProfileObjectType (..),
    newPutProfileObjectType,

    -- * Request Lenses
    putProfileObjectType_expirationDays,
    putProfileObjectType_templateId,
    putProfileObjectType_keys,
    putProfileObjectType_encryptionKey,
    putProfileObjectType_allowProfileCreation,
    putProfileObjectType_fields,
    putProfileObjectType_tags,
    putProfileObjectType_domainName,
    putProfileObjectType_objectTypeName,
    putProfileObjectType_description,

    -- * Destructuring the Response
    PutProfileObjectTypeResponse (..),
    newPutProfileObjectTypeResponse,

    -- * Response Lenses
    putProfileObjectTypeResponse_expirationDays,
    putProfileObjectTypeResponse_lastUpdatedAt,
    putProfileObjectTypeResponse_createdAt,
    putProfileObjectTypeResponse_templateId,
    putProfileObjectTypeResponse_keys,
    putProfileObjectTypeResponse_encryptionKey,
    putProfileObjectTypeResponse_allowProfileCreation,
    putProfileObjectTypeResponse_fields,
    putProfileObjectTypeResponse_tags,
    putProfileObjectTypeResponse_httpStatus,
    putProfileObjectTypeResponse_objectTypeName,
    putProfileObjectTypeResponse_description,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutProfileObjectType' smart constructor.
data PutProfileObjectType = PutProfileObjectType'
  { -- | The number of days until the data in the object expires.
    expirationDays :: Prelude.Maybe Prelude.Natural,
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
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text,
    -- | Description of the profile object type.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProfileObjectType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationDays', 'putProfileObjectType_expirationDays' - The number of days until the data in the object expires.
--
-- 'templateId', 'putProfileObjectType_templateId' - A unique identifier for the object template.
--
-- 'keys', 'putProfileObjectType_keys' - A list of unique keys that can be used to map data to the profile.
--
-- 'encryptionKey', 'putProfileObjectType_encryptionKey' - The customer-provided key to encrypt the profile object that will be
-- created in this profile object type.
--
-- 'allowProfileCreation', 'putProfileObjectType_allowProfileCreation' - Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
--
-- 'fields', 'putProfileObjectType_fields' - A map of the name and ObjectType field.
--
-- 'tags', 'putProfileObjectType_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'domainName', 'putProfileObjectType_domainName' - The unique name of the domain.
--
-- 'objectTypeName', 'putProfileObjectType_objectTypeName' - The name of the profile object type.
--
-- 'description', 'putProfileObjectType_description' - Description of the profile object type.
newPutProfileObjectType ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  PutProfileObjectType
newPutProfileObjectType
  pDomainName_
  pObjectTypeName_
  pDescription_ =
    PutProfileObjectType'
      { expirationDays =
          Prelude.Nothing,
        templateId = Prelude.Nothing,
        keys = Prelude.Nothing,
        encryptionKey = Prelude.Nothing,
        allowProfileCreation = Prelude.Nothing,
        fields = Prelude.Nothing,
        tags = Prelude.Nothing,
        domainName = pDomainName_,
        objectTypeName = pObjectTypeName_,
        description = pDescription_
      }

-- | The number of days until the data in the object expires.
putProfileObjectType_expirationDays :: Lens.Lens' PutProfileObjectType (Prelude.Maybe Prelude.Natural)
putProfileObjectType_expirationDays = Lens.lens (\PutProfileObjectType' {expirationDays} -> expirationDays) (\s@PutProfileObjectType' {} a -> s {expirationDays = a} :: PutProfileObjectType)

-- | A unique identifier for the object template.
putProfileObjectType_templateId :: Lens.Lens' PutProfileObjectType (Prelude.Maybe Prelude.Text)
putProfileObjectType_templateId = Lens.lens (\PutProfileObjectType' {templateId} -> templateId) (\s@PutProfileObjectType' {} a -> s {templateId = a} :: PutProfileObjectType)

-- | A list of unique keys that can be used to map data to the profile.
putProfileObjectType_keys :: Lens.Lens' PutProfileObjectType (Prelude.Maybe (Prelude.HashMap Prelude.Text [ObjectTypeKey]))
putProfileObjectType_keys = Lens.lens (\PutProfileObjectType' {keys} -> keys) (\s@PutProfileObjectType' {} a -> s {keys = a} :: PutProfileObjectType) Prelude.. Lens.mapping Lens.coerced

-- | The customer-provided key to encrypt the profile object that will be
-- created in this profile object type.
putProfileObjectType_encryptionKey :: Lens.Lens' PutProfileObjectType (Prelude.Maybe Prelude.Text)
putProfileObjectType_encryptionKey = Lens.lens (\PutProfileObjectType' {encryptionKey} -> encryptionKey) (\s@PutProfileObjectType' {} a -> s {encryptionKey = a} :: PutProfileObjectType)

-- | Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
putProfileObjectType_allowProfileCreation :: Lens.Lens' PutProfileObjectType (Prelude.Maybe Prelude.Bool)
putProfileObjectType_allowProfileCreation = Lens.lens (\PutProfileObjectType' {allowProfileCreation} -> allowProfileCreation) (\s@PutProfileObjectType' {} a -> s {allowProfileCreation = a} :: PutProfileObjectType)

-- | A map of the name and ObjectType field.
putProfileObjectType_fields :: Lens.Lens' PutProfileObjectType (Prelude.Maybe (Prelude.HashMap Prelude.Text ObjectTypeField))
putProfileObjectType_fields = Lens.lens (\PutProfileObjectType' {fields} -> fields) (\s@PutProfileObjectType' {} a -> s {fields = a} :: PutProfileObjectType) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
putProfileObjectType_tags :: Lens.Lens' PutProfileObjectType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putProfileObjectType_tags = Lens.lens (\PutProfileObjectType' {tags} -> tags) (\s@PutProfileObjectType' {} a -> s {tags = a} :: PutProfileObjectType) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the domain.
putProfileObjectType_domainName :: Lens.Lens' PutProfileObjectType Prelude.Text
putProfileObjectType_domainName = Lens.lens (\PutProfileObjectType' {domainName} -> domainName) (\s@PutProfileObjectType' {} a -> s {domainName = a} :: PutProfileObjectType)

-- | The name of the profile object type.
putProfileObjectType_objectTypeName :: Lens.Lens' PutProfileObjectType Prelude.Text
putProfileObjectType_objectTypeName = Lens.lens (\PutProfileObjectType' {objectTypeName} -> objectTypeName) (\s@PutProfileObjectType' {} a -> s {objectTypeName = a} :: PutProfileObjectType)

-- | Description of the profile object type.
putProfileObjectType_description :: Lens.Lens' PutProfileObjectType Prelude.Text
putProfileObjectType_description = Lens.lens (\PutProfileObjectType' {description} -> description) (\s@PutProfileObjectType' {} a -> s {description = a} :: PutProfileObjectType)

instance Core.AWSRequest PutProfileObjectType where
  type
    AWSResponse PutProfileObjectType =
      PutProfileObjectTypeResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutProfileObjectTypeResponse'
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

instance Prelude.Hashable PutProfileObjectType

instance Prelude.NFData PutProfileObjectType

instance Core.ToHeaders PutProfileObjectType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutProfileObjectType where
  toJSON PutProfileObjectType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExpirationDays" Core..=)
              Prelude.<$> expirationDays,
            ("TemplateId" Core..=) Prelude.<$> templateId,
            ("Keys" Core..=) Prelude.<$> keys,
            ("EncryptionKey" Core..=) Prelude.<$> encryptionKey,
            ("AllowProfileCreation" Core..=)
              Prelude.<$> allowProfileCreation,
            ("Fields" Core..=) Prelude.<$> fields,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Description" Core..= description)
          ]
      )

instance Core.ToPath PutProfileObjectType where
  toPath PutProfileObjectType' {..} =
    Prelude.mconcat
      [ "/domains/",
        Core.toBS domainName,
        "/object-types/",
        Core.toBS objectTypeName
      ]

instance Core.ToQuery PutProfileObjectType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutProfileObjectTypeResponse' smart constructor.
data PutProfileObjectTypeResponse = PutProfileObjectTypeResponse'
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
    -- | Description of the profile object type.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProfileObjectTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expirationDays', 'putProfileObjectTypeResponse_expirationDays' - The number of days until the data in the object expires.
--
-- 'lastUpdatedAt', 'putProfileObjectTypeResponse_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
--
-- 'createdAt', 'putProfileObjectTypeResponse_createdAt' - The timestamp of when the domain was created.
--
-- 'templateId', 'putProfileObjectTypeResponse_templateId' - A unique identifier for the object template.
--
-- 'keys', 'putProfileObjectTypeResponse_keys' - A list of unique keys that can be used to map data to the profile.
--
-- 'encryptionKey', 'putProfileObjectTypeResponse_encryptionKey' - The customer-provided key to encrypt the profile object that will be
-- created in this profile object type.
--
-- 'allowProfileCreation', 'putProfileObjectTypeResponse_allowProfileCreation' - Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
--
-- 'fields', 'putProfileObjectTypeResponse_fields' - A map of the name and ObjectType field.
--
-- 'tags', 'putProfileObjectTypeResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'putProfileObjectTypeResponse_httpStatus' - The response's http status code.
--
-- 'objectTypeName', 'putProfileObjectTypeResponse_objectTypeName' - The name of the profile object type.
--
-- 'description', 'putProfileObjectTypeResponse_description' - Description of the profile object type.
newPutProfileObjectTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  PutProfileObjectTypeResponse
newPutProfileObjectTypeResponse
  pHttpStatus_
  pObjectTypeName_
  pDescription_ =
    PutProfileObjectTypeResponse'
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
putProfileObjectTypeResponse_expirationDays :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe Prelude.Natural)
putProfileObjectTypeResponse_expirationDays = Lens.lens (\PutProfileObjectTypeResponse' {expirationDays} -> expirationDays) (\s@PutProfileObjectTypeResponse' {} a -> s {expirationDays = a} :: PutProfileObjectTypeResponse)

-- | The timestamp of when the domain was most recently edited.
putProfileObjectTypeResponse_lastUpdatedAt :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe Prelude.UTCTime)
putProfileObjectTypeResponse_lastUpdatedAt = Lens.lens (\PutProfileObjectTypeResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@PutProfileObjectTypeResponse' {} a -> s {lastUpdatedAt = a} :: PutProfileObjectTypeResponse) Prelude.. Lens.mapping Core._Time

-- | The timestamp of when the domain was created.
putProfileObjectTypeResponse_createdAt :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe Prelude.UTCTime)
putProfileObjectTypeResponse_createdAt = Lens.lens (\PutProfileObjectTypeResponse' {createdAt} -> createdAt) (\s@PutProfileObjectTypeResponse' {} a -> s {createdAt = a} :: PutProfileObjectTypeResponse) Prelude.. Lens.mapping Core._Time

-- | A unique identifier for the object template.
putProfileObjectTypeResponse_templateId :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe Prelude.Text)
putProfileObjectTypeResponse_templateId = Lens.lens (\PutProfileObjectTypeResponse' {templateId} -> templateId) (\s@PutProfileObjectTypeResponse' {} a -> s {templateId = a} :: PutProfileObjectTypeResponse)

-- | A list of unique keys that can be used to map data to the profile.
putProfileObjectTypeResponse_keys :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [ObjectTypeKey]))
putProfileObjectTypeResponse_keys = Lens.lens (\PutProfileObjectTypeResponse' {keys} -> keys) (\s@PutProfileObjectTypeResponse' {} a -> s {keys = a} :: PutProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The customer-provided key to encrypt the profile object that will be
-- created in this profile object type.
putProfileObjectTypeResponse_encryptionKey :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe Prelude.Text)
putProfileObjectTypeResponse_encryptionKey = Lens.lens (\PutProfileObjectTypeResponse' {encryptionKey} -> encryptionKey) (\s@PutProfileObjectTypeResponse' {} a -> s {encryptionKey = a} :: PutProfileObjectTypeResponse)

-- | Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
putProfileObjectTypeResponse_allowProfileCreation :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe Prelude.Bool)
putProfileObjectTypeResponse_allowProfileCreation = Lens.lens (\PutProfileObjectTypeResponse' {allowProfileCreation} -> allowProfileCreation) (\s@PutProfileObjectTypeResponse' {} a -> s {allowProfileCreation = a} :: PutProfileObjectTypeResponse)

-- | A map of the name and ObjectType field.
putProfileObjectTypeResponse_fields :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ObjectTypeField))
putProfileObjectTypeResponse_fields = Lens.lens (\PutProfileObjectTypeResponse' {fields} -> fields) (\s@PutProfileObjectTypeResponse' {} a -> s {fields = a} :: PutProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The tags used to organize, track, or control access for this resource.
putProfileObjectTypeResponse_tags :: Lens.Lens' PutProfileObjectTypeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putProfileObjectTypeResponse_tags = Lens.lens (\PutProfileObjectTypeResponse' {tags} -> tags) (\s@PutProfileObjectTypeResponse' {} a -> s {tags = a} :: PutProfileObjectTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putProfileObjectTypeResponse_httpStatus :: Lens.Lens' PutProfileObjectTypeResponse Prelude.Int
putProfileObjectTypeResponse_httpStatus = Lens.lens (\PutProfileObjectTypeResponse' {httpStatus} -> httpStatus) (\s@PutProfileObjectTypeResponse' {} a -> s {httpStatus = a} :: PutProfileObjectTypeResponse)

-- | The name of the profile object type.
putProfileObjectTypeResponse_objectTypeName :: Lens.Lens' PutProfileObjectTypeResponse Prelude.Text
putProfileObjectTypeResponse_objectTypeName = Lens.lens (\PutProfileObjectTypeResponse' {objectTypeName} -> objectTypeName) (\s@PutProfileObjectTypeResponse' {} a -> s {objectTypeName = a} :: PutProfileObjectTypeResponse)

-- | Description of the profile object type.
putProfileObjectTypeResponse_description :: Lens.Lens' PutProfileObjectTypeResponse Prelude.Text
putProfileObjectTypeResponse_description = Lens.lens (\PutProfileObjectTypeResponse' {description} -> description) (\s@PutProfileObjectTypeResponse' {} a -> s {description = a} :: PutProfileObjectTypeResponse)

instance Prelude.NFData PutProfileObjectTypeResponse
