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
-- Module      : Amazonka.IoTFleetHub.DescribeApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a Fleet Hub for AWS IoT Device Management web
-- application.
--
-- Fleet Hub for AWS IoT Device Management is in public preview and is
-- subject to change.
module Amazonka.IoTFleetHub.DescribeApplication
  ( -- * Creating a Request
    DescribeApplication (..),
    newDescribeApplication,

    -- * Request Lenses
    describeApplication_applicationId,

    -- * Destructuring the Response
    DescribeApplicationResponse (..),
    newDescribeApplicationResponse,

    -- * Response Lenses
    describeApplicationResponse_tags,
    describeApplicationResponse_ssoClientId,
    describeApplicationResponse_errorMessage,
    describeApplicationResponse_applicationDescription,
    describeApplicationResponse_httpStatus,
    describeApplicationResponse_applicationId,
    describeApplicationResponse_applicationArn,
    describeApplicationResponse_applicationName,
    describeApplicationResponse_applicationUrl,
    describeApplicationResponse_applicationState,
    describeApplicationResponse_applicationCreationDate,
    describeApplicationResponse_applicationLastUpdateDate,
    describeApplicationResponse_roleArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetHub.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplication' smart constructor.
data DescribeApplication = DescribeApplication'
  { -- | The unique Id of the web application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'describeApplication_applicationId' - The unique Id of the web application.
newDescribeApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  DescribeApplication
newDescribeApplication pApplicationId_ =
  DescribeApplication'
    { applicationId =
        pApplicationId_
    }

-- | The unique Id of the web application.
describeApplication_applicationId :: Lens.Lens' DescribeApplication Prelude.Text
describeApplication_applicationId = Lens.lens (\DescribeApplication' {applicationId} -> applicationId) (\s@DescribeApplication' {} a -> s {applicationId = a} :: DescribeApplication)

instance Core.AWSRequest DescribeApplication where
  type
    AWSResponse DescribeApplication =
      DescribeApplicationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ssoClientId")
            Prelude.<*> (x Core..?> "errorMessage")
            Prelude.<*> (x Core..?> "applicationDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "applicationId")
            Prelude.<*> (x Core..:> "applicationArn")
            Prelude.<*> (x Core..:> "applicationName")
            Prelude.<*> (x Core..:> "applicationUrl")
            Prelude.<*> (x Core..:> "applicationState")
            Prelude.<*> (x Core..:> "applicationCreationDate")
            Prelude.<*> (x Core..:> "applicationLastUpdateDate")
            Prelude.<*> (x Core..:> "roleArn")
      )

instance Prelude.Hashable DescribeApplication where
  hashWithSalt _salt DescribeApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DescribeApplication where
  rnf DescribeApplication' {..} =
    Prelude.rnf applicationId

instance Core.ToHeaders DescribeApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeApplication where
  toPath DescribeApplication' {..} =
    Prelude.mconcat
      ["/applications/", Core.toBS applicationId]

instance Core.ToQuery DescribeApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationResponse' smart constructor.
data DescribeApplicationResponse = DescribeApplicationResponse'
  { -- | A set of key\/value pairs that you can use to manage the web application
    -- resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Id of the single sign-on client that you use to authenticate and
    -- authorize users on the web application.
    ssoClientId :: Prelude.Maybe Prelude.Text,
    -- | A message indicating why the @DescribeApplication@ API failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | An optional description of the web application.
    applicationDescription :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique Id of the web application.
    applicationId :: Prelude.Text,
    -- | The ARN of the web application.
    applicationArn :: Prelude.Text,
    -- | The name of the web application.
    applicationName :: Prelude.Text,
    -- | The URL of the web application.
    applicationUrl :: Prelude.Text,
    -- | The current state of the web application.
    applicationState :: ApplicationState,
    -- | The date (in Unix epoch time) when the application was created.
    applicationCreationDate :: Prelude.Integer,
    -- | The date (in Unix epoch time) when the application was last updated.
    applicationLastUpdateDate :: Prelude.Integer,
    -- | The ARN of the role that the web application assumes when it interacts
    -- with AWS IoT Core.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeApplicationResponse_tags' - A set of key\/value pairs that you can use to manage the web application
-- resource.
--
-- 'ssoClientId', 'describeApplicationResponse_ssoClientId' - The Id of the single sign-on client that you use to authenticate and
-- authorize users on the web application.
--
-- 'errorMessage', 'describeApplicationResponse_errorMessage' - A message indicating why the @DescribeApplication@ API failed.
--
-- 'applicationDescription', 'describeApplicationResponse_applicationDescription' - An optional description of the web application.
--
-- 'httpStatus', 'describeApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationId', 'describeApplicationResponse_applicationId' - The unique Id of the web application.
--
-- 'applicationArn', 'describeApplicationResponse_applicationArn' - The ARN of the web application.
--
-- 'applicationName', 'describeApplicationResponse_applicationName' - The name of the web application.
--
-- 'applicationUrl', 'describeApplicationResponse_applicationUrl' - The URL of the web application.
--
-- 'applicationState', 'describeApplicationResponse_applicationState' - The current state of the web application.
--
-- 'applicationCreationDate', 'describeApplicationResponse_applicationCreationDate' - The date (in Unix epoch time) when the application was created.
--
-- 'applicationLastUpdateDate', 'describeApplicationResponse_applicationLastUpdateDate' - The date (in Unix epoch time) when the application was last updated.
--
-- 'roleArn', 'describeApplicationResponse_roleArn' - The ARN of the role that the web application assumes when it interacts
-- with AWS IoT Core.
newDescribeApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'applicationArn'
  Prelude.Text ->
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'applicationUrl'
  Prelude.Text ->
  -- | 'applicationState'
  ApplicationState ->
  -- | 'applicationCreationDate'
  Prelude.Integer ->
  -- | 'applicationLastUpdateDate'
  Prelude.Integer ->
  -- | 'roleArn'
  Prelude.Text ->
  DescribeApplicationResponse
newDescribeApplicationResponse
  pHttpStatus_
  pApplicationId_
  pApplicationArn_
  pApplicationName_
  pApplicationUrl_
  pApplicationState_
  pApplicationCreationDate_
  pApplicationLastUpdateDate_
  pRoleArn_ =
    DescribeApplicationResponse'
      { tags =
          Prelude.Nothing,
        ssoClientId = Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        applicationDescription = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        applicationId = pApplicationId_,
        applicationArn = pApplicationArn_,
        applicationName = pApplicationName_,
        applicationUrl = pApplicationUrl_,
        applicationState = pApplicationState_,
        applicationCreationDate =
          pApplicationCreationDate_,
        applicationLastUpdateDate =
          pApplicationLastUpdateDate_,
        roleArn = pRoleArn_
      }

-- | A set of key\/value pairs that you can use to manage the web application
-- resource.
describeApplicationResponse_tags :: Lens.Lens' DescribeApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeApplicationResponse_tags = Lens.lens (\DescribeApplicationResponse' {tags} -> tags) (\s@DescribeApplicationResponse' {} a -> s {tags = a} :: DescribeApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Id of the single sign-on client that you use to authenticate and
-- authorize users on the web application.
describeApplicationResponse_ssoClientId :: Lens.Lens' DescribeApplicationResponse (Prelude.Maybe Prelude.Text)
describeApplicationResponse_ssoClientId = Lens.lens (\DescribeApplicationResponse' {ssoClientId} -> ssoClientId) (\s@DescribeApplicationResponse' {} a -> s {ssoClientId = a} :: DescribeApplicationResponse)

-- | A message indicating why the @DescribeApplication@ API failed.
describeApplicationResponse_errorMessage :: Lens.Lens' DescribeApplicationResponse (Prelude.Maybe Prelude.Text)
describeApplicationResponse_errorMessage = Lens.lens (\DescribeApplicationResponse' {errorMessage} -> errorMessage) (\s@DescribeApplicationResponse' {} a -> s {errorMessage = a} :: DescribeApplicationResponse)

-- | An optional description of the web application.
describeApplicationResponse_applicationDescription :: Lens.Lens' DescribeApplicationResponse (Prelude.Maybe Prelude.Text)
describeApplicationResponse_applicationDescription = Lens.lens (\DescribeApplicationResponse' {applicationDescription} -> applicationDescription) (\s@DescribeApplicationResponse' {} a -> s {applicationDescription = a} :: DescribeApplicationResponse)

-- | The response's http status code.
describeApplicationResponse_httpStatus :: Lens.Lens' DescribeApplicationResponse Prelude.Int
describeApplicationResponse_httpStatus = Lens.lens (\DescribeApplicationResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationResponse' {} a -> s {httpStatus = a} :: DescribeApplicationResponse)

-- | The unique Id of the web application.
describeApplicationResponse_applicationId :: Lens.Lens' DescribeApplicationResponse Prelude.Text
describeApplicationResponse_applicationId = Lens.lens (\DescribeApplicationResponse' {applicationId} -> applicationId) (\s@DescribeApplicationResponse' {} a -> s {applicationId = a} :: DescribeApplicationResponse)

-- | The ARN of the web application.
describeApplicationResponse_applicationArn :: Lens.Lens' DescribeApplicationResponse Prelude.Text
describeApplicationResponse_applicationArn = Lens.lens (\DescribeApplicationResponse' {applicationArn} -> applicationArn) (\s@DescribeApplicationResponse' {} a -> s {applicationArn = a} :: DescribeApplicationResponse)

-- | The name of the web application.
describeApplicationResponse_applicationName :: Lens.Lens' DescribeApplicationResponse Prelude.Text
describeApplicationResponse_applicationName = Lens.lens (\DescribeApplicationResponse' {applicationName} -> applicationName) (\s@DescribeApplicationResponse' {} a -> s {applicationName = a} :: DescribeApplicationResponse)

-- | The URL of the web application.
describeApplicationResponse_applicationUrl :: Lens.Lens' DescribeApplicationResponse Prelude.Text
describeApplicationResponse_applicationUrl = Lens.lens (\DescribeApplicationResponse' {applicationUrl} -> applicationUrl) (\s@DescribeApplicationResponse' {} a -> s {applicationUrl = a} :: DescribeApplicationResponse)

-- | The current state of the web application.
describeApplicationResponse_applicationState :: Lens.Lens' DescribeApplicationResponse ApplicationState
describeApplicationResponse_applicationState = Lens.lens (\DescribeApplicationResponse' {applicationState} -> applicationState) (\s@DescribeApplicationResponse' {} a -> s {applicationState = a} :: DescribeApplicationResponse)

-- | The date (in Unix epoch time) when the application was created.
describeApplicationResponse_applicationCreationDate :: Lens.Lens' DescribeApplicationResponse Prelude.Integer
describeApplicationResponse_applicationCreationDate = Lens.lens (\DescribeApplicationResponse' {applicationCreationDate} -> applicationCreationDate) (\s@DescribeApplicationResponse' {} a -> s {applicationCreationDate = a} :: DescribeApplicationResponse)

-- | The date (in Unix epoch time) when the application was last updated.
describeApplicationResponse_applicationLastUpdateDate :: Lens.Lens' DescribeApplicationResponse Prelude.Integer
describeApplicationResponse_applicationLastUpdateDate = Lens.lens (\DescribeApplicationResponse' {applicationLastUpdateDate} -> applicationLastUpdateDate) (\s@DescribeApplicationResponse' {} a -> s {applicationLastUpdateDate = a} :: DescribeApplicationResponse)

-- | The ARN of the role that the web application assumes when it interacts
-- with AWS IoT Core.
describeApplicationResponse_roleArn :: Lens.Lens' DescribeApplicationResponse Prelude.Text
describeApplicationResponse_roleArn = Lens.lens (\DescribeApplicationResponse' {roleArn} -> roleArn) (\s@DescribeApplicationResponse' {} a -> s {roleArn = a} :: DescribeApplicationResponse)

instance Prelude.NFData DescribeApplicationResponse where
  rnf DescribeApplicationResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ssoClientId
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf applicationDescription
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf applicationUrl
      `Prelude.seq` Prelude.rnf applicationState
      `Prelude.seq` Prelude.rnf applicationCreationDate
      `Prelude.seq` Prelude.rnf applicationLastUpdateDate
      `Prelude.seq` Prelude.rnf roleArn
