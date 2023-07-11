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
-- Module      : Amazonka.CloudDirectory.GetAppliedSchemaVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns current applied schema version ARN, including the minor version
-- in use.
module Amazonka.CloudDirectory.GetAppliedSchemaVersion
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAppliedSchemaVersion' smart constructor.
data GetAppliedSchemaVersion = GetAppliedSchemaVersion'
  { -- | The ARN of the applied schema.
    schemaArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetAppliedSchemaVersion where
  type
    AWSResponse GetAppliedSchemaVersion =
      GetAppliedSchemaVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppliedSchemaVersionResponse'
            Prelude.<$> (x Data..?> "AppliedSchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAppliedSchemaVersion where
  hashWithSalt _salt GetAppliedSchemaVersion' {..} =
    _salt `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData GetAppliedSchemaVersion where
  rnf GetAppliedSchemaVersion' {..} =
    Prelude.rnf schemaArn

instance Data.ToHeaders GetAppliedSchemaVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetAppliedSchemaVersion where
  toJSON GetAppliedSchemaVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SchemaArn" Data..= schemaArn)]
      )

instance Data.ToPath GetAppliedSchemaVersion where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/getappliedschema"

instance Data.ToQuery GetAppliedSchemaVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppliedSchemaVersionResponse' smart constructor.
data GetAppliedSchemaVersionResponse = GetAppliedSchemaVersionResponse'
  { -- | Current applied schema ARN, including the minor version in use if one
    -- was provided.
    appliedSchemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf GetAppliedSchemaVersionResponse' {..} =
    Prelude.rnf appliedSchemaArn
      `Prelude.seq` Prelude.rnf httpStatus
