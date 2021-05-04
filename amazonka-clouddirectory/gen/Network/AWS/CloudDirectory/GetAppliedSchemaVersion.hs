{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAppliedSchemaVersion' smart constructor.
data GetAppliedSchemaVersion = GetAppliedSchemaVersion'
  { -- | The ARN of the applied schema.
    schemaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetAppliedSchemaVersion
newGetAppliedSchemaVersion pSchemaArn_ =
  GetAppliedSchemaVersion' {schemaArn = pSchemaArn_}

-- | The ARN of the applied schema.
getAppliedSchemaVersion_schemaArn :: Lens.Lens' GetAppliedSchemaVersion Prelude.Text
getAppliedSchemaVersion_schemaArn = Lens.lens (\GetAppliedSchemaVersion' {schemaArn} -> schemaArn) (\s@GetAppliedSchemaVersion' {} a -> s {schemaArn = a} :: GetAppliedSchemaVersion)

instance Prelude.AWSRequest GetAppliedSchemaVersion where
  type
    Rs GetAppliedSchemaVersion =
      GetAppliedSchemaVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppliedSchemaVersionResponse'
            Prelude.<$> (x Prelude..?> "AppliedSchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAppliedSchemaVersion

instance Prelude.NFData GetAppliedSchemaVersion

instance Prelude.ToHeaders GetAppliedSchemaVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON GetAppliedSchemaVersion where
  toJSON GetAppliedSchemaVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("SchemaArn" Prelude..= schemaArn)]
      )

instance Prelude.ToPath GetAppliedSchemaVersion where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/getappliedschema"

instance Prelude.ToQuery GetAppliedSchemaVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppliedSchemaVersionResponse' smart constructor.
data GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse'
  { -- | Current applied schema ARN, including the minor version in use if one
    -- was provided.
    appliedSchemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetAppliedSchemaVersionResponse
newGetAppliedSchemaVersionResponse pHttpStatus_ =
  GetAppliedSchemaVersionResponse'
    { appliedSchemaArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Current applied schema ARN, including the minor version in use if one
-- was provided.
getAppliedSchemaVersionResponse_appliedSchemaArn :: Lens.Lens' GetAppliedSchemaVersionResponse (Prelude.Maybe Prelude.Text)
getAppliedSchemaVersionResponse_appliedSchemaArn = Lens.lens (\GetAppliedSchemaVersionResponse' {appliedSchemaArn} -> appliedSchemaArn) (\s@GetAppliedSchemaVersionResponse' {} a -> s {appliedSchemaArn = a} :: GetAppliedSchemaVersionResponse)

-- | The response's http status code.
getAppliedSchemaVersionResponse_httpStatus :: Lens.Lens' GetAppliedSchemaVersionResponse Prelude.Int
getAppliedSchemaVersionResponse_httpStatus = Lens.lens (\GetAppliedSchemaVersionResponse' {httpStatus} -> httpStatus) (\s@GetAppliedSchemaVersionResponse' {} a -> s {httpStatus = a} :: GetAppliedSchemaVersionResponse)

instance
  Prelude.NFData
    GetAppliedSchemaVersionResponse
