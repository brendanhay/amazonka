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
-- Module      : Network.AWS.Greengrass.GetDeviceDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition version.
module Network.AWS.Greengrass.GetDeviceDefinitionVersion
  ( -- * Creating a Request
    GetDeviceDefinitionVersion (..),
    newGetDeviceDefinitionVersion,

    -- * Request Lenses
    getDeviceDefinitionVersion_nextToken,
    getDeviceDefinitionVersion_deviceDefinitionVersionId,
    getDeviceDefinitionVersion_deviceDefinitionId,

    -- * Destructuring the Response
    GetDeviceDefinitionVersionResponse (..),
    newGetDeviceDefinitionVersionResponse,

    -- * Response Lenses
    getDeviceDefinitionVersionResponse_creationTimestamp,
    getDeviceDefinitionVersionResponse_nextToken,
    getDeviceDefinitionVersionResponse_arn,
    getDeviceDefinitionVersionResponse_id,
    getDeviceDefinitionVersionResponse_version,
    getDeviceDefinitionVersionResponse_definition,
    getDeviceDefinitionVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDeviceDefinitionVersion' smart constructor.
data GetDeviceDefinitionVersion = GetDeviceDefinitionVersion'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device definition version. This value maps to the
    -- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
    -- object, which is returned by \'\'ListDeviceDefinitionVersions\'\'
    -- requests. If the version is the last one that was associated with a
    -- device definition, the value also maps to the \'\'LatestVersion\'\'
    -- property of the corresponding \'\'DefinitionInformation\'\' object.
    deviceDefinitionVersionId :: Prelude.Text,
    -- | The ID of the device definition.
    deviceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDeviceDefinitionVersion_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'deviceDefinitionVersionId', 'getDeviceDefinitionVersion_deviceDefinitionVersionId' - The ID of the device definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListDeviceDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- device definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
--
-- 'deviceDefinitionId', 'getDeviceDefinitionVersion_deviceDefinitionId' - The ID of the device definition.
newGetDeviceDefinitionVersion ::
  -- | 'deviceDefinitionVersionId'
  Prelude.Text ->
  -- | 'deviceDefinitionId'
  Prelude.Text ->
  GetDeviceDefinitionVersion
newGetDeviceDefinitionVersion
  pDeviceDefinitionVersionId_
  pDeviceDefinitionId_ =
    GetDeviceDefinitionVersion'
      { nextToken =
          Prelude.Nothing,
        deviceDefinitionVersionId =
          pDeviceDefinitionVersionId_,
        deviceDefinitionId = pDeviceDefinitionId_
      }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getDeviceDefinitionVersion_nextToken :: Lens.Lens' GetDeviceDefinitionVersion (Prelude.Maybe Prelude.Text)
getDeviceDefinitionVersion_nextToken = Lens.lens (\GetDeviceDefinitionVersion' {nextToken} -> nextToken) (\s@GetDeviceDefinitionVersion' {} a -> s {nextToken = a} :: GetDeviceDefinitionVersion)

-- | The ID of the device definition version. This value maps to the
-- \'\'Version\'\' property of the corresponding \'\'VersionInformation\'\'
-- object, which is returned by \'\'ListDeviceDefinitionVersions\'\'
-- requests. If the version is the last one that was associated with a
-- device definition, the value also maps to the \'\'LatestVersion\'\'
-- property of the corresponding \'\'DefinitionInformation\'\' object.
getDeviceDefinitionVersion_deviceDefinitionVersionId :: Lens.Lens' GetDeviceDefinitionVersion Prelude.Text
getDeviceDefinitionVersion_deviceDefinitionVersionId = Lens.lens (\GetDeviceDefinitionVersion' {deviceDefinitionVersionId} -> deviceDefinitionVersionId) (\s@GetDeviceDefinitionVersion' {} a -> s {deviceDefinitionVersionId = a} :: GetDeviceDefinitionVersion)

-- | The ID of the device definition.
getDeviceDefinitionVersion_deviceDefinitionId :: Lens.Lens' GetDeviceDefinitionVersion Prelude.Text
getDeviceDefinitionVersion_deviceDefinitionId = Lens.lens (\GetDeviceDefinitionVersion' {deviceDefinitionId} -> deviceDefinitionId) (\s@GetDeviceDefinitionVersion' {} a -> s {deviceDefinitionId = a} :: GetDeviceDefinitionVersion)

instance Core.AWSRequest GetDeviceDefinitionVersion where
  type
    AWSResponse GetDeviceDefinitionVersion =
      GetDeviceDefinitionVersionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceDefinitionVersionResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "Definition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeviceDefinitionVersion

instance Prelude.NFData GetDeviceDefinitionVersion

instance Core.ToHeaders GetDeviceDefinitionVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDeviceDefinitionVersion where
  toPath GetDeviceDefinitionVersion' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/devices/",
        Core.toBS deviceDefinitionId,
        "/versions/",
        Core.toBS deviceDefinitionVersionId
      ]

instance Core.ToQuery GetDeviceDefinitionVersion where
  toQuery GetDeviceDefinitionVersion' {..} =
    Prelude.mconcat ["NextToken" Core.=: nextToken]

-- | /See:/ 'newGetDeviceDefinitionVersionResponse' smart constructor.
data GetDeviceDefinitionVersionResponse = GetDeviceDefinitionVersionResponse'
  { -- | The time, in milliseconds since the epoch, when the device definition
    -- version was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the device definition version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device definition version.
    id :: Prelude.Maybe Prelude.Text,
    -- | The version of the device definition version.
    version :: Prelude.Maybe Prelude.Text,
    -- | Information about the device definition version.
    definition :: Prelude.Maybe DeviceDefinitionVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceDefinitionVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getDeviceDefinitionVersionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the device definition
-- version was created.
--
-- 'nextToken', 'getDeviceDefinitionVersionResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'arn', 'getDeviceDefinitionVersionResponse_arn' - The ARN of the device definition version.
--
-- 'id', 'getDeviceDefinitionVersionResponse_id' - The ID of the device definition version.
--
-- 'version', 'getDeviceDefinitionVersionResponse_version' - The version of the device definition version.
--
-- 'definition', 'getDeviceDefinitionVersionResponse_definition' - Information about the device definition version.
--
-- 'httpStatus', 'getDeviceDefinitionVersionResponse_httpStatus' - The response's http status code.
newGetDeviceDefinitionVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeviceDefinitionVersionResponse
newGetDeviceDefinitionVersionResponse pHttpStatus_ =
  GetDeviceDefinitionVersionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing,
      definition = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the device definition
-- version was created.
getDeviceDefinitionVersionResponse_creationTimestamp :: Lens.Lens' GetDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionVersionResponse_creationTimestamp = Lens.lens (\GetDeviceDefinitionVersionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetDeviceDefinitionVersionResponse' {} a -> s {creationTimestamp = a} :: GetDeviceDefinitionVersionResponse)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
getDeviceDefinitionVersionResponse_nextToken :: Lens.Lens' GetDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionVersionResponse_nextToken = Lens.lens (\GetDeviceDefinitionVersionResponse' {nextToken} -> nextToken) (\s@GetDeviceDefinitionVersionResponse' {} a -> s {nextToken = a} :: GetDeviceDefinitionVersionResponse)

-- | The ARN of the device definition version.
getDeviceDefinitionVersionResponse_arn :: Lens.Lens' GetDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionVersionResponse_arn = Lens.lens (\GetDeviceDefinitionVersionResponse' {arn} -> arn) (\s@GetDeviceDefinitionVersionResponse' {} a -> s {arn = a} :: GetDeviceDefinitionVersionResponse)

-- | The ID of the device definition version.
getDeviceDefinitionVersionResponse_id :: Lens.Lens' GetDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionVersionResponse_id = Lens.lens (\GetDeviceDefinitionVersionResponse' {id} -> id) (\s@GetDeviceDefinitionVersionResponse' {} a -> s {id = a} :: GetDeviceDefinitionVersionResponse)

-- | The version of the device definition version.
getDeviceDefinitionVersionResponse_version :: Lens.Lens' GetDeviceDefinitionVersionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionVersionResponse_version = Lens.lens (\GetDeviceDefinitionVersionResponse' {version} -> version) (\s@GetDeviceDefinitionVersionResponse' {} a -> s {version = a} :: GetDeviceDefinitionVersionResponse)

-- | Information about the device definition version.
getDeviceDefinitionVersionResponse_definition :: Lens.Lens' GetDeviceDefinitionVersionResponse (Prelude.Maybe DeviceDefinitionVersion)
getDeviceDefinitionVersionResponse_definition = Lens.lens (\GetDeviceDefinitionVersionResponse' {definition} -> definition) (\s@GetDeviceDefinitionVersionResponse' {} a -> s {definition = a} :: GetDeviceDefinitionVersionResponse)

-- | The response's http status code.
getDeviceDefinitionVersionResponse_httpStatus :: Lens.Lens' GetDeviceDefinitionVersionResponse Prelude.Int
getDeviceDefinitionVersionResponse_httpStatus = Lens.lens (\GetDeviceDefinitionVersionResponse' {httpStatus} -> httpStatus) (\s@GetDeviceDefinitionVersionResponse' {} a -> s {httpStatus = a} :: GetDeviceDefinitionVersionResponse)

instance
  Prelude.NFData
    GetDeviceDefinitionVersionResponse
