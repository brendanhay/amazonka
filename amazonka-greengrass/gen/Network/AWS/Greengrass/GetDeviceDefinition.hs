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
-- Module      : Network.AWS.Greengrass.GetDeviceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a device definition.
module Network.AWS.Greengrass.GetDeviceDefinition
  ( -- * Creating a Request
    GetDeviceDefinition (..),
    newGetDeviceDefinition,

    -- * Request Lenses
    getDeviceDefinition_deviceDefinitionId,

    -- * Destructuring the Response
    GetDeviceDefinitionResponse (..),
    newGetDeviceDefinitionResponse,

    -- * Response Lenses
    getDeviceDefinitionResponse_creationTimestamp,
    getDeviceDefinitionResponse_latestVersionArn,
    getDeviceDefinitionResponse_latestVersion,
    getDeviceDefinitionResponse_arn,
    getDeviceDefinitionResponse_id,
    getDeviceDefinitionResponse_name,
    getDeviceDefinitionResponse_lastUpdatedTimestamp,
    getDeviceDefinitionResponse_tags,
    getDeviceDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDeviceDefinition' smart constructor.
data GetDeviceDefinition = GetDeviceDefinition'
  { -- | The ID of the device definition.
    deviceDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceDefinitionId', 'getDeviceDefinition_deviceDefinitionId' - The ID of the device definition.
newGetDeviceDefinition ::
  -- | 'deviceDefinitionId'
  Prelude.Text ->
  GetDeviceDefinition
newGetDeviceDefinition pDeviceDefinitionId_ =
  GetDeviceDefinition'
    { deviceDefinitionId =
        pDeviceDefinitionId_
    }

-- | The ID of the device definition.
getDeviceDefinition_deviceDefinitionId :: Lens.Lens' GetDeviceDefinition Prelude.Text
getDeviceDefinition_deviceDefinitionId = Lens.lens (\GetDeviceDefinition' {deviceDefinitionId} -> deviceDefinitionId) (\s@GetDeviceDefinition' {} a -> s {deviceDefinitionId = a} :: GetDeviceDefinition)

instance Core.AWSRequest GetDeviceDefinition where
  type
    AWSResponse GetDeviceDefinition =
      GetDeviceDefinitionResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeviceDefinitionResponse'
            Prelude.<$> (x Core..?> "CreationTimestamp")
            Prelude.<*> (x Core..?> "LatestVersionArn")
            Prelude.<*> (x Core..?> "LatestVersion")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "LastUpdatedTimestamp")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeviceDefinition

instance Prelude.NFData GetDeviceDefinition

instance Core.ToHeaders GetDeviceDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDeviceDefinition where
  toPath GetDeviceDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/devices/",
        Core.toBS deviceDefinitionId
      ]

instance Core.ToQuery GetDeviceDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeviceDefinitionResponse' smart constructor.
data GetDeviceDefinitionResponse = GetDeviceDefinitionResponse'
  { -- | The time, in milliseconds since the epoch, when the definition was
    -- created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the definition.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the definition.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | Tag(s) attached to the resource arn.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeviceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getDeviceDefinitionResponse_creationTimestamp' - The time, in milliseconds since the epoch, when the definition was
-- created.
--
-- 'latestVersionArn', 'getDeviceDefinitionResponse_latestVersionArn' - The ARN of the latest version associated with the definition.
--
-- 'latestVersion', 'getDeviceDefinitionResponse_latestVersion' - The ID of the latest version associated with the definition.
--
-- 'arn', 'getDeviceDefinitionResponse_arn' - The ARN of the definition.
--
-- 'id', 'getDeviceDefinitionResponse_id' - The ID of the definition.
--
-- 'name', 'getDeviceDefinitionResponse_name' - The name of the definition.
--
-- 'lastUpdatedTimestamp', 'getDeviceDefinitionResponse_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last
-- updated.
--
-- 'tags', 'getDeviceDefinitionResponse_tags' - Tag(s) attached to the resource arn.
--
-- 'httpStatus', 'getDeviceDefinitionResponse_httpStatus' - The response's http status code.
newGetDeviceDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeviceDefinitionResponse
newGetDeviceDefinitionResponse pHttpStatus_ =
  GetDeviceDefinitionResponse'
    { creationTimestamp =
        Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the definition was
-- created.
getDeviceDefinitionResponse_creationTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionResponse_creationTimestamp = Lens.lens (\GetDeviceDefinitionResponse' {creationTimestamp} -> creationTimestamp) (\s@GetDeviceDefinitionResponse' {} a -> s {creationTimestamp = a} :: GetDeviceDefinitionResponse)

-- | The ARN of the latest version associated with the definition.
getDeviceDefinitionResponse_latestVersionArn :: Lens.Lens' GetDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionResponse_latestVersionArn = Lens.lens (\GetDeviceDefinitionResponse' {latestVersionArn} -> latestVersionArn) (\s@GetDeviceDefinitionResponse' {} a -> s {latestVersionArn = a} :: GetDeviceDefinitionResponse)

-- | The ID of the latest version associated with the definition.
getDeviceDefinitionResponse_latestVersion :: Lens.Lens' GetDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionResponse_latestVersion = Lens.lens (\GetDeviceDefinitionResponse' {latestVersion} -> latestVersion) (\s@GetDeviceDefinitionResponse' {} a -> s {latestVersion = a} :: GetDeviceDefinitionResponse)

-- | The ARN of the definition.
getDeviceDefinitionResponse_arn :: Lens.Lens' GetDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionResponse_arn = Lens.lens (\GetDeviceDefinitionResponse' {arn} -> arn) (\s@GetDeviceDefinitionResponse' {} a -> s {arn = a} :: GetDeviceDefinitionResponse)

-- | The ID of the definition.
getDeviceDefinitionResponse_id :: Lens.Lens' GetDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionResponse_id = Lens.lens (\GetDeviceDefinitionResponse' {id} -> id) (\s@GetDeviceDefinitionResponse' {} a -> s {id = a} :: GetDeviceDefinitionResponse)

-- | The name of the definition.
getDeviceDefinitionResponse_name :: Lens.Lens' GetDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionResponse_name = Lens.lens (\GetDeviceDefinitionResponse' {name} -> name) (\s@GetDeviceDefinitionResponse' {} a -> s {name = a} :: GetDeviceDefinitionResponse)

-- | The time, in milliseconds since the epoch, when the definition was last
-- updated.
getDeviceDefinitionResponse_lastUpdatedTimestamp :: Lens.Lens' GetDeviceDefinitionResponse (Prelude.Maybe Prelude.Text)
getDeviceDefinitionResponse_lastUpdatedTimestamp = Lens.lens (\GetDeviceDefinitionResponse' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GetDeviceDefinitionResponse' {} a -> s {lastUpdatedTimestamp = a} :: GetDeviceDefinitionResponse)

-- | Tag(s) attached to the resource arn.
getDeviceDefinitionResponse_tags :: Lens.Lens' GetDeviceDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getDeviceDefinitionResponse_tags = Lens.lens (\GetDeviceDefinitionResponse' {tags} -> tags) (\s@GetDeviceDefinitionResponse' {} a -> s {tags = a} :: GetDeviceDefinitionResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDeviceDefinitionResponse_httpStatus :: Lens.Lens' GetDeviceDefinitionResponse Prelude.Int
getDeviceDefinitionResponse_httpStatus = Lens.lens (\GetDeviceDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetDeviceDefinitionResponse' {} a -> s {httpStatus = a} :: GetDeviceDefinitionResponse)

instance Prelude.NFData GetDeviceDefinitionResponse
