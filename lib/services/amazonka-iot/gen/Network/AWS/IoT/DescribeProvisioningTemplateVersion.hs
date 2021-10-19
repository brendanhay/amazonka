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
-- Module      : Network.AWS.IoT.DescribeProvisioningTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template version.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeProvisioningTemplateVersion>
-- action.
module Network.AWS.IoT.DescribeProvisioningTemplateVersion
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
    describeProvisioningTemplateVersionResponse_versionId,
    describeProvisioningTemplateVersionResponse_creationDate,
    describeProvisioningTemplateVersionResponse_templateBody,
    describeProvisioningTemplateVersionResponse_isDefaultVersion,
    describeProvisioningTemplateVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeProvisioningTemplateVersion' smart constructor.
data DescribeProvisioningTemplateVersion = DescribeProvisioningTemplateVersion'
  { -- | The template name.
    templateName :: Prelude.Text,
    -- | The fleet provisioning template version ID.
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
-- 'versionId', 'describeProvisioningTemplateVersion_versionId' - The fleet provisioning template version ID.
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

-- | The fleet provisioning template version ID.
describeProvisioningTemplateVersion_versionId :: Lens.Lens' DescribeProvisioningTemplateVersion Prelude.Int
describeProvisioningTemplateVersion_versionId = Lens.lens (\DescribeProvisioningTemplateVersion' {versionId} -> versionId) (\s@DescribeProvisioningTemplateVersion' {} a -> s {versionId = a} :: DescribeProvisioningTemplateVersion)

instance
  Core.AWSRequest
    DescribeProvisioningTemplateVersion
  where
  type
    AWSResponse DescribeProvisioningTemplateVersion =
      DescribeProvisioningTemplateVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateVersionResponse'
            Prelude.<$> (x Core..?> "versionId")
              Prelude.<*> (x Core..?> "creationDate")
              Prelude.<*> (x Core..?> "templateBody")
              Prelude.<*> (x Core..?> "isDefaultVersion")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeProvisioningTemplateVersion

instance
  Prelude.NFData
    DescribeProvisioningTemplateVersion

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
  { -- | The fleet provisioning template version ID.
    versionId :: Prelude.Maybe Prelude.Int,
    -- | The date when the fleet provisioning template version was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The JSON formatted contents of the fleet provisioning template version.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | True if the fleet provisioning template version is the default version.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
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
-- 'versionId', 'describeProvisioningTemplateVersionResponse_versionId' - The fleet provisioning template version ID.
--
-- 'creationDate', 'describeProvisioningTemplateVersionResponse_creationDate' - The date when the fleet provisioning template version was created.
--
-- 'templateBody', 'describeProvisioningTemplateVersionResponse_templateBody' - The JSON formatted contents of the fleet provisioning template version.
--
-- 'isDefaultVersion', 'describeProvisioningTemplateVersionResponse_isDefaultVersion' - True if the fleet provisioning template version is the default version.
--
-- 'httpStatus', 'describeProvisioningTemplateVersionResponse_httpStatus' - The response's http status code.
newDescribeProvisioningTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisioningTemplateVersionResponse
newDescribeProvisioningTemplateVersionResponse
  pHttpStatus_ =
    DescribeProvisioningTemplateVersionResponse'
      { versionId =
          Prelude.Nothing,
        creationDate = Prelude.Nothing,
        templateBody = Prelude.Nothing,
        isDefaultVersion =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The fleet provisioning template version ID.
describeProvisioningTemplateVersionResponse_versionId :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Int)
describeProvisioningTemplateVersionResponse_versionId = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {versionId} -> versionId) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {versionId = a} :: DescribeProvisioningTemplateVersionResponse)

-- | The date when the fleet provisioning template version was created.
describeProvisioningTemplateVersionResponse_creationDate :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.UTCTime)
describeProvisioningTemplateVersionResponse_creationDate = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {creationDate} -> creationDate) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {creationDate = a} :: DescribeProvisioningTemplateVersionResponse) Prelude.. Lens.mapping Core._Time

-- | The JSON formatted contents of the fleet provisioning template version.
describeProvisioningTemplateVersionResponse_templateBody :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateVersionResponse_templateBody = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {templateBody} -> templateBody) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {templateBody = a} :: DescribeProvisioningTemplateVersionResponse)

-- | True if the fleet provisioning template version is the default version.
describeProvisioningTemplateVersionResponse_isDefaultVersion :: Lens.Lens' DescribeProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Bool)
describeProvisioningTemplateVersionResponse_isDefaultVersion = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {isDefaultVersion} -> isDefaultVersion) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {isDefaultVersion = a} :: DescribeProvisioningTemplateVersionResponse)

-- | The response's http status code.
describeProvisioningTemplateVersionResponse_httpStatus :: Lens.Lens' DescribeProvisioningTemplateVersionResponse Prelude.Int
describeProvisioningTemplateVersionResponse_httpStatus = Lens.lens (\DescribeProvisioningTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisioningTemplateVersionResponse' {} a -> s {httpStatus = a} :: DescribeProvisioningTemplateVersionResponse)

instance
  Prelude.NFData
    DescribeProvisioningTemplateVersionResponse
