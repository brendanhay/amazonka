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
-- Module      : Amazonka.Panorama.DescribeApplicationInstanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an application instance\'s configuration
-- manifest.
module Amazonka.Panorama.DescribeApplicationInstanceDetails
  ( -- * Creating a Request
    DescribeApplicationInstanceDetails (..),
    newDescribeApplicationInstanceDetails,

    -- * Request Lenses
    describeApplicationInstanceDetails_applicationInstanceId,

    -- * Destructuring the Response
    DescribeApplicationInstanceDetailsResponse (..),
    newDescribeApplicationInstanceDetailsResponse,

    -- * Response Lenses
    describeApplicationInstanceDetailsResponse_applicationInstanceId,
    describeApplicationInstanceDetailsResponse_applicationInstanceIdToReplace,
    describeApplicationInstanceDetailsResponse_createdTime,
    describeApplicationInstanceDetailsResponse_defaultRuntimeContextDevice,
    describeApplicationInstanceDetailsResponse_description,
    describeApplicationInstanceDetailsResponse_manifestOverridesPayload,
    describeApplicationInstanceDetailsResponse_manifestPayload,
    describeApplicationInstanceDetailsResponse_name,
    describeApplicationInstanceDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeApplicationInstanceDetails' smart constructor.
data DescribeApplicationInstanceDetails = DescribeApplicationInstanceDetails'
  { -- | The application instance\'s ID.
    applicationInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInstanceId', 'describeApplicationInstanceDetails_applicationInstanceId' - The application instance\'s ID.
newDescribeApplicationInstanceDetails ::
  -- | 'applicationInstanceId'
  Prelude.Text ->
  DescribeApplicationInstanceDetails
newDescribeApplicationInstanceDetails
  pApplicationInstanceId_ =
    DescribeApplicationInstanceDetails'
      { applicationInstanceId =
          pApplicationInstanceId_
      }

-- | The application instance\'s ID.
describeApplicationInstanceDetails_applicationInstanceId :: Lens.Lens' DescribeApplicationInstanceDetails Prelude.Text
describeApplicationInstanceDetails_applicationInstanceId = Lens.lens (\DescribeApplicationInstanceDetails' {applicationInstanceId} -> applicationInstanceId) (\s@DescribeApplicationInstanceDetails' {} a -> s {applicationInstanceId = a} :: DescribeApplicationInstanceDetails)

instance
  Core.AWSRequest
    DescribeApplicationInstanceDetails
  where
  type
    AWSResponse DescribeApplicationInstanceDetails =
      DescribeApplicationInstanceDetailsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicationInstanceDetailsResponse'
            Prelude.<$> (x Data..?> "ApplicationInstanceId")
            Prelude.<*> (x Data..?> "ApplicationInstanceIdToReplace")
            Prelude.<*> (x Data..?> "CreatedTime")
            Prelude.<*> (x Data..?> "DefaultRuntimeContextDevice")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "ManifestOverridesPayload")
            Prelude.<*> (x Data..?> "ManifestPayload")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeApplicationInstanceDetails
  where
  hashWithSalt
    _salt
    DescribeApplicationInstanceDetails' {..} =
      _salt `Prelude.hashWithSalt` applicationInstanceId

instance
  Prelude.NFData
    DescribeApplicationInstanceDetails
  where
  rnf DescribeApplicationInstanceDetails' {..} =
    Prelude.rnf applicationInstanceId

instance
  Data.ToHeaders
    DescribeApplicationInstanceDetails
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DescribeApplicationInstanceDetails
  where
  toPath DescribeApplicationInstanceDetails' {..} =
    Prelude.mconcat
      [ "/application-instances/",
        Data.toBS applicationInstanceId,
        "/details"
      ]

instance
  Data.ToQuery
    DescribeApplicationInstanceDetails
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeApplicationInstanceDetailsResponse' smart constructor.
data DescribeApplicationInstanceDetailsResponse = DescribeApplicationInstanceDetailsResponse'
  { -- | The application instance\'s ID.
    applicationInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application instance that this instance replaced.
    applicationInstanceIdToReplace :: Prelude.Maybe Prelude.Text,
    -- | When the application instance was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The application instance\'s default runtime context device.
    defaultRuntimeContextDevice :: Prelude.Maybe Prelude.Text,
    -- | The application instance\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Parameter overrides for the configuration manifest.
    manifestOverridesPayload :: Prelude.Maybe ManifestOverridesPayload,
    -- | The application instance\'s configuration manifest.
    manifestPayload :: Prelude.Maybe ManifestPayload,
    -- | The application instance\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicationInstanceDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInstanceId', 'describeApplicationInstanceDetailsResponse_applicationInstanceId' - The application instance\'s ID.
--
-- 'applicationInstanceIdToReplace', 'describeApplicationInstanceDetailsResponse_applicationInstanceIdToReplace' - The ID of the application instance that this instance replaced.
--
-- 'createdTime', 'describeApplicationInstanceDetailsResponse_createdTime' - When the application instance was created.
--
-- 'defaultRuntimeContextDevice', 'describeApplicationInstanceDetailsResponse_defaultRuntimeContextDevice' - The application instance\'s default runtime context device.
--
-- 'description', 'describeApplicationInstanceDetailsResponse_description' - The application instance\'s description.
--
-- 'manifestOverridesPayload', 'describeApplicationInstanceDetailsResponse_manifestOverridesPayload' - Parameter overrides for the configuration manifest.
--
-- 'manifestPayload', 'describeApplicationInstanceDetailsResponse_manifestPayload' - The application instance\'s configuration manifest.
--
-- 'name', 'describeApplicationInstanceDetailsResponse_name' - The application instance\'s name.
--
-- 'httpStatus', 'describeApplicationInstanceDetailsResponse_httpStatus' - The response's http status code.
newDescribeApplicationInstanceDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApplicationInstanceDetailsResponse
newDescribeApplicationInstanceDetailsResponse
  pHttpStatus_ =
    DescribeApplicationInstanceDetailsResponse'
      { applicationInstanceId =
          Prelude.Nothing,
        applicationInstanceIdToReplace =
          Prelude.Nothing,
        createdTime = Prelude.Nothing,
        defaultRuntimeContextDevice =
          Prelude.Nothing,
        description = Prelude.Nothing,
        manifestOverridesPayload =
          Prelude.Nothing,
        manifestPayload =
          Prelude.Nothing,
        name = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The application instance\'s ID.
describeApplicationInstanceDetailsResponse_applicationInstanceId :: Lens.Lens' DescribeApplicationInstanceDetailsResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceDetailsResponse_applicationInstanceId = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {applicationInstanceId} -> applicationInstanceId) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {applicationInstanceId = a} :: DescribeApplicationInstanceDetailsResponse)

-- | The ID of the application instance that this instance replaced.
describeApplicationInstanceDetailsResponse_applicationInstanceIdToReplace :: Lens.Lens' DescribeApplicationInstanceDetailsResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceDetailsResponse_applicationInstanceIdToReplace = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {applicationInstanceIdToReplace} -> applicationInstanceIdToReplace) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {applicationInstanceIdToReplace = a} :: DescribeApplicationInstanceDetailsResponse)

-- | When the application instance was created.
describeApplicationInstanceDetailsResponse_createdTime :: Lens.Lens' DescribeApplicationInstanceDetailsResponse (Prelude.Maybe Prelude.UTCTime)
describeApplicationInstanceDetailsResponse_createdTime = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {createdTime} -> createdTime) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {createdTime = a} :: DescribeApplicationInstanceDetailsResponse) Prelude.. Lens.mapping Data._Time

-- | The application instance\'s default runtime context device.
describeApplicationInstanceDetailsResponse_defaultRuntimeContextDevice :: Lens.Lens' DescribeApplicationInstanceDetailsResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceDetailsResponse_defaultRuntimeContextDevice = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {defaultRuntimeContextDevice} -> defaultRuntimeContextDevice) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {defaultRuntimeContextDevice = a} :: DescribeApplicationInstanceDetailsResponse)

-- | The application instance\'s description.
describeApplicationInstanceDetailsResponse_description :: Lens.Lens' DescribeApplicationInstanceDetailsResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceDetailsResponse_description = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {description} -> description) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {description = a} :: DescribeApplicationInstanceDetailsResponse)

-- | Parameter overrides for the configuration manifest.
describeApplicationInstanceDetailsResponse_manifestOverridesPayload :: Lens.Lens' DescribeApplicationInstanceDetailsResponse (Prelude.Maybe ManifestOverridesPayload)
describeApplicationInstanceDetailsResponse_manifestOverridesPayload = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {manifestOverridesPayload} -> manifestOverridesPayload) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {manifestOverridesPayload = a} :: DescribeApplicationInstanceDetailsResponse)

-- | The application instance\'s configuration manifest.
describeApplicationInstanceDetailsResponse_manifestPayload :: Lens.Lens' DescribeApplicationInstanceDetailsResponse (Prelude.Maybe ManifestPayload)
describeApplicationInstanceDetailsResponse_manifestPayload = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {manifestPayload} -> manifestPayload) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {manifestPayload = a} :: DescribeApplicationInstanceDetailsResponse)

-- | The application instance\'s name.
describeApplicationInstanceDetailsResponse_name :: Lens.Lens' DescribeApplicationInstanceDetailsResponse (Prelude.Maybe Prelude.Text)
describeApplicationInstanceDetailsResponse_name = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {name} -> name) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {name = a} :: DescribeApplicationInstanceDetailsResponse)

-- | The response's http status code.
describeApplicationInstanceDetailsResponse_httpStatus :: Lens.Lens' DescribeApplicationInstanceDetailsResponse Prelude.Int
describeApplicationInstanceDetailsResponse_httpStatus = Lens.lens (\DescribeApplicationInstanceDetailsResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicationInstanceDetailsResponse' {} a -> s {httpStatus = a} :: DescribeApplicationInstanceDetailsResponse)

instance
  Prelude.NFData
    DescribeApplicationInstanceDetailsResponse
  where
  rnf DescribeApplicationInstanceDetailsResponse' {..} =
    Prelude.rnf applicationInstanceId
      `Prelude.seq` Prelude.rnf applicationInstanceIdToReplace
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf defaultRuntimeContextDevice
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf manifestOverridesPayload
      `Prelude.seq` Prelude.rnf manifestPayload
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
