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
-- Module      : Amazonka.CustomerProfiles.GetProfileObjectTypeTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the template information for a specific object type.
--
-- A template is a predefined ProfileObjectType, such as
-- “Salesforce-Account” or “Salesforce-Contact.” When a user sends a
-- ProfileObject, using the PutProfileObject API, with an ObjectTypeName
-- that matches one of the TemplateIds, it uses the mappings from the
-- template.
module Amazonka.CustomerProfiles.GetProfileObjectTypeTemplate
  ( -- * Creating a Request
    GetProfileObjectTypeTemplate (..),
    newGetProfileObjectTypeTemplate,

    -- * Request Lenses
    getProfileObjectTypeTemplate_templateId,

    -- * Destructuring the Response
    GetProfileObjectTypeTemplateResponse (..),
    newGetProfileObjectTypeTemplateResponse,

    -- * Response Lenses
    getProfileObjectTypeTemplateResponse_allowProfileCreation,
    getProfileObjectTypeTemplateResponse_fields,
    getProfileObjectTypeTemplateResponse_keys,
    getProfileObjectTypeTemplateResponse_sourceLastUpdatedTimestampFormat,
    getProfileObjectTypeTemplateResponse_sourceName,
    getProfileObjectTypeTemplateResponse_sourceObject,
    getProfileObjectTypeTemplateResponse_templateId,
    getProfileObjectTypeTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProfileObjectTypeTemplate' smart constructor.
data GetProfileObjectTypeTemplate = GetProfileObjectTypeTemplate'
  { -- | A unique identifier for the object template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfileObjectTypeTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateId', 'getProfileObjectTypeTemplate_templateId' - A unique identifier for the object template.
newGetProfileObjectTypeTemplate ::
  -- | 'templateId'
  Prelude.Text ->
  GetProfileObjectTypeTemplate
newGetProfileObjectTypeTemplate pTemplateId_ =
  GetProfileObjectTypeTemplate'
    { templateId =
        pTemplateId_
    }

-- | A unique identifier for the object template.
getProfileObjectTypeTemplate_templateId :: Lens.Lens' GetProfileObjectTypeTemplate Prelude.Text
getProfileObjectTypeTemplate_templateId = Lens.lens (\GetProfileObjectTypeTemplate' {templateId} -> templateId) (\s@GetProfileObjectTypeTemplate' {} a -> s {templateId = a} :: GetProfileObjectTypeTemplate)

instance Core.AWSRequest GetProfileObjectTypeTemplate where
  type
    AWSResponse GetProfileObjectTypeTemplate =
      GetProfileObjectTypeTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProfileObjectTypeTemplateResponse'
            Prelude.<$> (x Data..?> "AllowProfileCreation")
            Prelude.<*> (x Data..?> "Fields" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Keys" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "SourceLastUpdatedTimestampFormat")
            Prelude.<*> (x Data..?> "SourceName")
            Prelude.<*> (x Data..?> "SourceObject")
            Prelude.<*> (x Data..?> "TemplateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetProfileObjectTypeTemplate
  where
  hashWithSalt _salt GetProfileObjectTypeTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateId

instance Prelude.NFData GetProfileObjectTypeTemplate where
  rnf GetProfileObjectTypeTemplate' {..} =
    Prelude.rnf templateId

instance Data.ToHeaders GetProfileObjectTypeTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetProfileObjectTypeTemplate where
  toPath GetProfileObjectTypeTemplate' {..} =
    Prelude.mconcat
      ["/templates/", Data.toBS templateId]

instance Data.ToQuery GetProfileObjectTypeTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProfileObjectTypeTemplateResponse' smart constructor.
data GetProfileObjectTypeTemplateResponse = GetProfileObjectTypeTemplateResponse'
  { -- | Indicates whether a profile should be created when data is received if
    -- one doesn’t exist for an object of this type. The default is @FALSE@. If
    -- the AllowProfileCreation flag is set to @FALSE@, then the service tries
    -- to fetch a standard profile and associate this object with the profile.
    -- If it is set to @TRUE@, and if no match is found, then the service
    -- creates a new standard profile.
    allowProfileCreation :: Prelude.Maybe Prelude.Bool,
    -- | A map of the name and ObjectType field.
    fields :: Prelude.Maybe (Prelude.HashMap Prelude.Text ObjectTypeField),
    -- | A list of unique keys that can be used to map data to the profile.
    keys :: Prelude.Maybe (Prelude.HashMap Prelude.Text [ObjectTypeKey]),
    -- | The format of your @sourceLastUpdatedTimestamp@ that was previously set
    -- up.
    sourceLastUpdatedTimestampFormat :: Prelude.Maybe Prelude.Text,
    -- | The name of the source of the object template.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | The source of the object template.
    sourceObject :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the object template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfileObjectTypeTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowProfileCreation', 'getProfileObjectTypeTemplateResponse_allowProfileCreation' - Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
--
-- 'fields', 'getProfileObjectTypeTemplateResponse_fields' - A map of the name and ObjectType field.
--
-- 'keys', 'getProfileObjectTypeTemplateResponse_keys' - A list of unique keys that can be used to map data to the profile.
--
-- 'sourceLastUpdatedTimestampFormat', 'getProfileObjectTypeTemplateResponse_sourceLastUpdatedTimestampFormat' - The format of your @sourceLastUpdatedTimestamp@ that was previously set
-- up.
--
-- 'sourceName', 'getProfileObjectTypeTemplateResponse_sourceName' - The name of the source of the object template.
--
-- 'sourceObject', 'getProfileObjectTypeTemplateResponse_sourceObject' - The source of the object template.
--
-- 'templateId', 'getProfileObjectTypeTemplateResponse_templateId' - A unique identifier for the object template.
--
-- 'httpStatus', 'getProfileObjectTypeTemplateResponse_httpStatus' - The response's http status code.
newGetProfileObjectTypeTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProfileObjectTypeTemplateResponse
newGetProfileObjectTypeTemplateResponse pHttpStatus_ =
  GetProfileObjectTypeTemplateResponse'
    { allowProfileCreation =
        Prelude.Nothing,
      fields = Prelude.Nothing,
      keys = Prelude.Nothing,
      sourceLastUpdatedTimestampFormat =
        Prelude.Nothing,
      sourceName = Prelude.Nothing,
      sourceObject = Prelude.Nothing,
      templateId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether a profile should be created when data is received if
-- one doesn’t exist for an object of this type. The default is @FALSE@. If
-- the AllowProfileCreation flag is set to @FALSE@, then the service tries
-- to fetch a standard profile and associate this object with the profile.
-- If it is set to @TRUE@, and if no match is found, then the service
-- creates a new standard profile.
getProfileObjectTypeTemplateResponse_allowProfileCreation :: Lens.Lens' GetProfileObjectTypeTemplateResponse (Prelude.Maybe Prelude.Bool)
getProfileObjectTypeTemplateResponse_allowProfileCreation = Lens.lens (\GetProfileObjectTypeTemplateResponse' {allowProfileCreation} -> allowProfileCreation) (\s@GetProfileObjectTypeTemplateResponse' {} a -> s {allowProfileCreation = a} :: GetProfileObjectTypeTemplateResponse)

-- | A map of the name and ObjectType field.
getProfileObjectTypeTemplateResponse_fields :: Lens.Lens' GetProfileObjectTypeTemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ObjectTypeField))
getProfileObjectTypeTemplateResponse_fields = Lens.lens (\GetProfileObjectTypeTemplateResponse' {fields} -> fields) (\s@GetProfileObjectTypeTemplateResponse' {} a -> s {fields = a} :: GetProfileObjectTypeTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of unique keys that can be used to map data to the profile.
getProfileObjectTypeTemplateResponse_keys :: Lens.Lens' GetProfileObjectTypeTemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [ObjectTypeKey]))
getProfileObjectTypeTemplateResponse_keys = Lens.lens (\GetProfileObjectTypeTemplateResponse' {keys} -> keys) (\s@GetProfileObjectTypeTemplateResponse' {} a -> s {keys = a} :: GetProfileObjectTypeTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The format of your @sourceLastUpdatedTimestamp@ that was previously set
-- up.
getProfileObjectTypeTemplateResponse_sourceLastUpdatedTimestampFormat :: Lens.Lens' GetProfileObjectTypeTemplateResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeTemplateResponse_sourceLastUpdatedTimestampFormat = Lens.lens (\GetProfileObjectTypeTemplateResponse' {sourceLastUpdatedTimestampFormat} -> sourceLastUpdatedTimestampFormat) (\s@GetProfileObjectTypeTemplateResponse' {} a -> s {sourceLastUpdatedTimestampFormat = a} :: GetProfileObjectTypeTemplateResponse)

-- | The name of the source of the object template.
getProfileObjectTypeTemplateResponse_sourceName :: Lens.Lens' GetProfileObjectTypeTemplateResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeTemplateResponse_sourceName = Lens.lens (\GetProfileObjectTypeTemplateResponse' {sourceName} -> sourceName) (\s@GetProfileObjectTypeTemplateResponse' {} a -> s {sourceName = a} :: GetProfileObjectTypeTemplateResponse)

-- | The source of the object template.
getProfileObjectTypeTemplateResponse_sourceObject :: Lens.Lens' GetProfileObjectTypeTemplateResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeTemplateResponse_sourceObject = Lens.lens (\GetProfileObjectTypeTemplateResponse' {sourceObject} -> sourceObject) (\s@GetProfileObjectTypeTemplateResponse' {} a -> s {sourceObject = a} :: GetProfileObjectTypeTemplateResponse)

-- | A unique identifier for the object template.
getProfileObjectTypeTemplateResponse_templateId :: Lens.Lens' GetProfileObjectTypeTemplateResponse (Prelude.Maybe Prelude.Text)
getProfileObjectTypeTemplateResponse_templateId = Lens.lens (\GetProfileObjectTypeTemplateResponse' {templateId} -> templateId) (\s@GetProfileObjectTypeTemplateResponse' {} a -> s {templateId = a} :: GetProfileObjectTypeTemplateResponse)

-- | The response's http status code.
getProfileObjectTypeTemplateResponse_httpStatus :: Lens.Lens' GetProfileObjectTypeTemplateResponse Prelude.Int
getProfileObjectTypeTemplateResponse_httpStatus = Lens.lens (\GetProfileObjectTypeTemplateResponse' {httpStatus} -> httpStatus) (\s@GetProfileObjectTypeTemplateResponse' {} a -> s {httpStatus = a} :: GetProfileObjectTypeTemplateResponse)

instance
  Prelude.NFData
    GetProfileObjectTypeTemplateResponse
  where
  rnf GetProfileObjectTypeTemplateResponse' {..} =
    Prelude.rnf allowProfileCreation `Prelude.seq`
      Prelude.rnf fields `Prelude.seq`
        Prelude.rnf keys `Prelude.seq`
          Prelude.rnf sourceLastUpdatedTimestampFormat `Prelude.seq`
            Prelude.rnf sourceName `Prelude.seq`
              Prelude.rnf sourceObject `Prelude.seq`
                Prelude.rnf templateId `Prelude.seq`
                  Prelude.rnf httpStatus
