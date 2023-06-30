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
-- Module      : Amazonka.Pinpoint.GetVoiceTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages
-- that are sent through the voice channel.
module Amazonka.Pinpoint.GetVoiceTemplate
  ( -- * Creating a Request
    GetVoiceTemplate (..),
    newGetVoiceTemplate,

    -- * Request Lenses
    getVoiceTemplate_version,
    getVoiceTemplate_templateName,

    -- * Destructuring the Response
    GetVoiceTemplateResponse (..),
    newGetVoiceTemplateResponse,

    -- * Response Lenses
    getVoiceTemplateResponse_httpStatus,
    getVoiceTemplateResponse_voiceTemplateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceTemplate' smart constructor.
data GetVoiceTemplate = GetVoiceTemplate'
  { -- | The unique identifier for the version of the message template to update,
    -- retrieve information about, or delete. To retrieve identifiers and other
    -- information for all the versions of a template, use the Template
    -- Versions resource.
    --
    -- If specified, this value must match the identifier for an existing
    -- template version. If specified for an update operation, this value must
    -- match the identifier for the latest existing version of the template.
    -- This restriction helps ensure that race conditions don\'t occur.
    --
    -- If you don\'t specify a value for this parameter, Amazon Pinpoint does
    -- the following:
    --
    -- -   For a get operation, retrieves information about the active version
    --     of the template.
    --
    -- -   For an update operation, saves the updates to (overwrites) the
    --     latest existing version of the template, if the create-new-version
    --     parameter isn\'t used or is set to false.
    --
    -- -   For a delete operation, deletes the template, including all versions
    --     of the template.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'getVoiceTemplate_version' - The unique identifier for the version of the message template to update,
-- retrieve information about, or delete. To retrieve identifiers and other
-- information for all the versions of a template, use the Template
-- Versions resource.
--
-- If specified, this value must match the identifier for an existing
-- template version. If specified for an update operation, this value must
-- match the identifier for the latest existing version of the template.
-- This restriction helps ensure that race conditions don\'t occur.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint does
-- the following:
--
-- -   For a get operation, retrieves information about the active version
--     of the template.
--
-- -   For an update operation, saves the updates to (overwrites) the
--     latest existing version of the template, if the create-new-version
--     parameter isn\'t used or is set to false.
--
-- -   For a delete operation, deletes the template, including all versions
--     of the template.
--
-- 'templateName', 'getVoiceTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newGetVoiceTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  GetVoiceTemplate
newGetVoiceTemplate pTemplateName_ =
  GetVoiceTemplate'
    { version = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update,
-- retrieve information about, or delete. To retrieve identifiers and other
-- information for all the versions of a template, use the Template
-- Versions resource.
--
-- If specified, this value must match the identifier for an existing
-- template version. If specified for an update operation, this value must
-- match the identifier for the latest existing version of the template.
-- This restriction helps ensure that race conditions don\'t occur.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint does
-- the following:
--
-- -   For a get operation, retrieves information about the active version
--     of the template.
--
-- -   For an update operation, saves the updates to (overwrites) the
--     latest existing version of the template, if the create-new-version
--     parameter isn\'t used or is set to false.
--
-- -   For a delete operation, deletes the template, including all versions
--     of the template.
getVoiceTemplate_version :: Lens.Lens' GetVoiceTemplate (Prelude.Maybe Prelude.Text)
getVoiceTemplate_version = Lens.lens (\GetVoiceTemplate' {version} -> version) (\s@GetVoiceTemplate' {} a -> s {version = a} :: GetVoiceTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
getVoiceTemplate_templateName :: Lens.Lens' GetVoiceTemplate Prelude.Text
getVoiceTemplate_templateName = Lens.lens (\GetVoiceTemplate' {templateName} -> templateName) (\s@GetVoiceTemplate' {} a -> s {templateName = a} :: GetVoiceTemplate)

instance Core.AWSRequest GetVoiceTemplate where
  type
    AWSResponse GetVoiceTemplate =
      GetVoiceTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetVoiceTemplate where
  hashWithSalt _salt GetVoiceTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData GetVoiceTemplate where
  rnf GetVoiceTemplate' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders GetVoiceTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetVoiceTemplate where
  toPath GetVoiceTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Data.toBS templateName, "/voice"]

instance Data.ToQuery GetVoiceTemplate where
  toQuery GetVoiceTemplate' {..} =
    Prelude.mconcat ["version" Data.=: version]

-- | /See:/ 'newGetVoiceTemplateResponse' smart constructor.
data GetVoiceTemplateResponse = GetVoiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    voiceTemplateResponse :: VoiceTemplateResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getVoiceTemplateResponse_httpStatus' - The response's http status code.
--
-- 'voiceTemplateResponse', 'getVoiceTemplateResponse_voiceTemplateResponse' - Undocumented member.
newGetVoiceTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'voiceTemplateResponse'
  VoiceTemplateResponse ->
  GetVoiceTemplateResponse
newGetVoiceTemplateResponse
  pHttpStatus_
  pVoiceTemplateResponse_ =
    GetVoiceTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        voiceTemplateResponse = pVoiceTemplateResponse_
      }

-- | The response's http status code.
getVoiceTemplateResponse_httpStatus :: Lens.Lens' GetVoiceTemplateResponse Prelude.Int
getVoiceTemplateResponse_httpStatus = Lens.lens (\GetVoiceTemplateResponse' {httpStatus} -> httpStatus) (\s@GetVoiceTemplateResponse' {} a -> s {httpStatus = a} :: GetVoiceTemplateResponse)

-- | Undocumented member.
getVoiceTemplateResponse_voiceTemplateResponse :: Lens.Lens' GetVoiceTemplateResponse VoiceTemplateResponse
getVoiceTemplateResponse_voiceTemplateResponse = Lens.lens (\GetVoiceTemplateResponse' {voiceTemplateResponse} -> voiceTemplateResponse) (\s@GetVoiceTemplateResponse' {} a -> s {voiceTemplateResponse = a} :: GetVoiceTemplateResponse)

instance Prelude.NFData GetVoiceTemplateResponse where
  rnf GetVoiceTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf voiceTemplateResponse
