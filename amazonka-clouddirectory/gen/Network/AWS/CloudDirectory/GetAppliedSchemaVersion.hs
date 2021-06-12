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
-- Module      : Network.AWS.CloudDirectory.GetAppliedSchemaVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns current applied schema version ARN, including the minor version
-- in use.
module Network.AWS.CloudDirectory.GetAppliedSchemaVersion
  ( -- * Creating a Request
    GetAppliedSchemaVersion (..),
    newGetAppliedSchemaVersion,

    -- * Request Lenses
    getAppliedSchemaVersion_schemaArn,

    -- * Destructuring the Response
    GetAppliedSchemaVersionResponse (..),
    newGetAppliedSchemaVersionResponse,

    -- * Response Lenses
    getAppliedSchemaVersionResponse_appliedSchemaArn,
    getAppliedSchemaVersionResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAppliedSchemaVersion' smart constructor.
data GetAppliedSchemaVersion = GetAppliedSchemaVersion'
  { -- | The ARN of the applied schema.
    schemaArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppliedSchemaVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'getAppliedSchemaVersion_schemaArn' - The ARN of the applied schema.
newGetAppliedSchemaVersion ::
  -- | 'schemaArn'
  Core.Text ->
  GetAppliedSchemaVersion
newGetAppliedSchemaVersion pSchemaArn_ =
  GetAppliedSchemaVersion' {schemaArn = pSchemaArn_}

-- | The ARN of the applied schema.
getAppliedSchemaVersion_schemaArn :: Lens.Lens' GetAppliedSchemaVersion Core.Text
getAppliedSchemaVersion_schemaArn = Lens.lens (\GetAppliedSchemaVersion' {schemaArn} -> schemaArn) (\s@GetAppliedSchemaVersion' {} a -> s {schemaArn = a} :: GetAppliedSchemaVersion)

instance Core.AWSRequest GetAppliedSchemaVersion where
  type
    AWSResponse GetAppliedSchemaVersion =
      GetAppliedSchemaVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppliedSchemaVersionResponse'
            Core.<$> (x Core..?> "AppliedSchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAppliedSchemaVersion

instance Core.NFData GetAppliedSchemaVersion

instance Core.ToHeaders GetAppliedSchemaVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetAppliedSchemaVersion where
  toJSON GetAppliedSchemaVersion' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SchemaArn" Core..= schemaArn)]
      )

instance Core.ToPath GetAppliedSchemaVersion where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/schema/getappliedschema"

instance Core.ToQuery GetAppliedSchemaVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAppliedSchemaVersionResponse' smart constructor.
data GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse'
  { -- | Current applied schema ARN, including the minor version in use if one
    -- was provided.
    appliedSchemaArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppliedSchemaVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appliedSchemaArn', 'getAppliedSchemaVersionResponse_appliedSchemaArn' - Current applied schema ARN, including the minor version in use if one
-- was provided.
--
-- 'httpStatus', 'getAppliedSchemaVersionResponse_httpStatus' - The response's http status code.
newGetAppliedSchemaVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAppliedSchemaVersionResponse
newGetAppliedSchemaVersionResponse pHttpStatus_ =
  GetAppliedSchemaVersionResponse'
    { appliedSchemaArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Current applied schema ARN, including the minor version in use if one
-- was provided.
getAppliedSchemaVersionResponse_appliedSchemaArn :: Lens.Lens' GetAppliedSchemaVersionResponse (Core.Maybe Core.Text)
getAppliedSchemaVersionResponse_appliedSchemaArn = Lens.lens (\GetAppliedSchemaVersionResponse' {appliedSchemaArn} -> appliedSchemaArn) (\s@GetAppliedSchemaVersionResponse' {} a -> s {appliedSchemaArn = a} :: GetAppliedSchemaVersionResponse)

-- | The response's http status code.
getAppliedSchemaVersionResponse_httpStatus :: Lens.Lens' GetAppliedSchemaVersionResponse Core.Int
getAppliedSchemaVersionResponse_httpStatus = Lens.lens (\GetAppliedSchemaVersionResponse' {httpStatus} -> httpStatus) (\s@GetAppliedSchemaVersionResponse' {} a -> s {httpStatus = a} :: GetAppliedSchemaVersionResponse)

instance Core.NFData GetAppliedSchemaVersionResponse
