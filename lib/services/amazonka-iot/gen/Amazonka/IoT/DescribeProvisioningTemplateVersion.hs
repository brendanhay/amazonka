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
-- Module      : Amazonka.IoT.DescribeProvisioningTemplateVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a provisioning template version.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeProvisioningTemplateVersion>
-- action.
module Amazonka.IoT.DescribeProvisioningTemplateVersion
  ( -- * Creating a Request
    DescribeProvisioningTemplateVersion (..),
    newDescribeProvisioningTemplateVersion,

    -- * Request Lenses
    describeProvisioningTemplateVersion_templateName,
    describeProvisioningTemplateVersion_versionId,

    -- * Destructuring the Response
    DescribeProvisioningTemplateVersionResponse (..),
    newDescribeProvisioningTemplateVersionResponse,

    -- * Response Lenses
    describeProvisioningTemplateVersionResponse_isDefaultVersion,
    describeProvisioningTemplateVersionResponse_templateBody,
    describeProvisioningTemplateVersionResponse_creationDate,
    describeProvisioningTemplateVersionResponse_versionId,
    describeProvisioningTemplateVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProvisioningTemplateVersion' smart constructor.
data DescribeProvisioningTemplateVersion = DescribeProvisioningTemplateVersion'
  { -- | The template name.
    templateName :: Prelude.Text,
    -- | The provisioning template version ID.
    versionId :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisioningTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'describeProvisioningTemplateVersion_templateName' - The template name.
--
-- 'versionId', 'describeProvisioningTemplateVersion_versionId' - The provisioning template version ID.
newDescribeProvisioningTemplateVersion ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Int ->
  DescribeProvisioningTemplateVersion
newDescribeProvisioningTemplateVersion
  pTemplateName_
  pVersionId_ =
    DescribeProvisioningTemplateVersion'
      { templateName =
          pTemplateName_,
        versionId = pVersionId_
      }

-- | The template name.
describeProvisioningTemplateVersion_templateName :: Lens.Lens' DescribeProvisioningTemplateVersion Prelude.Text
describeProvisioningTemplateVersion_templateName = Lens.lens (\DescribeProvisioningTemplateVersion' {templateName} -> templateName) (\s@DescribeProvisioningTemplateVersion' {} a -> s {templateName = a} :: DescribeProvisioningTemplateVersion)

-- | The provisioning template version ID.
describeProvisioningTemplateVersion_versionId :: Lens.Lens' DescribeProvisioningTemplateVersion Prelude.Int
describeProvisioningTemplateVersion_versionId = Lens.lens (\DescribeProvisioningTemplateVersion' {versionId} -> versionId) (\s@DescribeProvisioningTemplateVersion' {} a -> s {versionId = a} :: DescribeProvisioningTemplateVersion)

instance
  Core.AWSRequest
    DescribeProvisioningTemplateVersion
  where
  type
    AWSResponse DescribeProvisioningTemplateVersion =
      DescribeProvisioningTemplateVersionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateVersionResponse'
            Prelude.<$> (x Core..?> "isDefaultVersion")
              Prelude.<*> (x Core..?> "templateBody")
              Prelude.<*> (x Core..?> "creationDate")
              Prelude.<*> (x Core..?> "versionId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeProvisioningTemplateVersion
  where
  hashWithSalt
    _salt
    DescribeProvisioningTemplateVersion' {..} =
      _salt `Prelude.hashWithSalt` templateName
        `Prelude.hashWithSalt` versionId

instance
  Prelude.NFData
    DescribeProvisioningTemplateVersion
  where
  rnf DescribeProvisioningTemplateVersion' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf versionId

instance
  Core.ToHeaders
    DescribeProvisioningTemplateVersion
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DescribeProvisioningTemplateVersion
  where
  toPath DescribeProvisioningTemplateVersion' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Core.toBS templateName,
        "/versions/",
        Core.toBS versionId
      ]

instance
  Core.ToQuery
    DescribeProvisioningTemplateVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisioningTemplateVersionResponse' smart constructor.
data DescribeProvisioningTemplateVersionResponse = DescribeProvisioningTemplateVersionResponse'
  { -- | True if the provisioning template version is the default version.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The JSON formatted contents of the provisioning template version.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The date when the provisioning template version was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The provisioning template version ID.
    versionId :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisioningTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefaultVersion', 'describeProvisioningTemplateVersionResponse_isDefaultVersion' - True if the provisioning template version is the default version.
--
-- 'templateBody', 'describeProvisioningTemplateVersionResponse_templateBody' - The JSON formatted contents of the provisioning template version.
--
-- 'creationDate', 'describeProvisioningTemplateVersionResponse_creationDate' - The date when the provisioning template version was created.
--
-- 'versionId', 'describeProvisioningTemplateVersionResponse_versionId' - The provisioning template version ID.
--
-- 'httpStatus', 'describeProvisioningTemplateVersionResponse_httpStatus' - The response's http status code.
newDescribeProvisioningTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisioningTemplateVersionResponse
newDescribeProvisioningTemplateVersionResponse
  pHttpStatus_ =
    DescribeProvisioningTemplateVersionResponse'
      { isDefaultVersion =
          Prelude.Nothing,
        templateBody = Prelude.Nothing,
        creationDate = Prelude.Nothing,
        versionId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | True if the provisioning template version is the default version.
describeProvisioningTemplateVersionResponse_isDefaultVersion :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Bool)
describeProvisioningTemplateVersionResponse_isDefaultVersion = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {isDefaultVersion} -> isDefaultVersion) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {isDefaultVersion = a} :: DescribeProvisioningTemplateVersionResponse)

-- | The JSON formatted contents of the provisioning template version.
describeProvisioningTemplateVersionResponse_templateBody :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateVersionResponse_templateBody = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {templateBody} -> templateBody) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {templateBody = a} :: DescribeProvisioningTemplateVersionResponse)

-- | The date when the provisioning template version was created.
describeProvisioningTemplateVersionResponse_creationDate :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.UTCTime)
describeProvisioningTemplateVersionResponse_creationDate = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {creationDate} -> creationDate) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {creationDate = a} :: DescribeProvisioningTemplateVersionResponse) Prelude.. Lens.mapping Core._Time

-- | The provisioning template version ID.
describeProvisioningTemplateVersionResponse_versionId :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Int)
describeProvisioningTemplateVersionResponse_versionId = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {versionId} -> versionId) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {versionId = a} :: DescribeProvisioningTemplateVersionResponse)

-- | The response's http status code.
describeProvisioningTemplateVersionResponse_httpStatus :: Lens.Lens' DescribeProvisioningTemplateVersionResponse Prelude.Int
describeProvisioningTemplateVersionResponse_httpStatus = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {httpStatus = a} :: DescribeProvisioningTemplateVersionResponse)

instance
  Prelude.NFData
    DescribeProvisioningTemplateVersionResponse
  where
  rnf DescribeProvisioningTemplateVersionResponse' {..} =
    Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
