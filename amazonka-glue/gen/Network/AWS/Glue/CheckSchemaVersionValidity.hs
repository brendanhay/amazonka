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
-- Module      : Network.AWS.Glue.CheckSchemaVersionValidity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the supplied schema. This call has no side effects, it simply
-- validates using the supplied schema using @DataFormat@ as the format.
-- Since it does not take a schema set name, no compatibility checks are
-- performed.
module Network.AWS.Glue.CheckSchemaVersionValidity
  ( -- * Creating a Request
    CheckSchemaVersionValidity (..),
    newCheckSchemaVersionValidity,

    -- * Request Lenses
    checkSchemaVersionValidity_dataFormat,
    checkSchemaVersionValidity_schemaDefinition,

    -- * Destructuring the Response
    CheckSchemaVersionValidityResponse (..),
    newCheckSchemaVersionValidityResponse,

    -- * Response Lenses
    checkSchemaVersionValidityResponse_valid,
    checkSchemaVersionValidityResponse_error,
    checkSchemaVersionValidityResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCheckSchemaVersionValidity' smart constructor.
data CheckSchemaVersionValidity = CheckSchemaVersionValidity'
  { -- | The data format of the schema definition. Currently only @AVRO@ is
    -- supported.
    dataFormat :: DataFormat,
    -- | The definition of the schema that has to be validated.
    schemaDefinition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CheckSchemaVersionValidity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataFormat', 'checkSchemaVersionValidity_dataFormat' - The data format of the schema definition. Currently only @AVRO@ is
-- supported.
--
-- 'schemaDefinition', 'checkSchemaVersionValidity_schemaDefinition' - The definition of the schema that has to be validated.
newCheckSchemaVersionValidity ::
  -- | 'dataFormat'
  DataFormat ->
  -- | 'schemaDefinition'
  Core.Text ->
  CheckSchemaVersionValidity
newCheckSchemaVersionValidity
  pDataFormat_
  pSchemaDefinition_ =
    CheckSchemaVersionValidity'
      { dataFormat =
          pDataFormat_,
        schemaDefinition = pSchemaDefinition_
      }

-- | The data format of the schema definition. Currently only @AVRO@ is
-- supported.
checkSchemaVersionValidity_dataFormat :: Lens.Lens' CheckSchemaVersionValidity DataFormat
checkSchemaVersionValidity_dataFormat = Lens.lens (\CheckSchemaVersionValidity' {dataFormat} -> dataFormat) (\s@CheckSchemaVersionValidity' {} a -> s {dataFormat = a} :: CheckSchemaVersionValidity)

-- | The definition of the schema that has to be validated.
checkSchemaVersionValidity_schemaDefinition :: Lens.Lens' CheckSchemaVersionValidity Core.Text
checkSchemaVersionValidity_schemaDefinition = Lens.lens (\CheckSchemaVersionValidity' {schemaDefinition} -> schemaDefinition) (\s@CheckSchemaVersionValidity' {} a -> s {schemaDefinition = a} :: CheckSchemaVersionValidity)

instance Core.AWSRequest CheckSchemaVersionValidity where
  type
    AWSResponse CheckSchemaVersionValidity =
      CheckSchemaVersionValidityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CheckSchemaVersionValidityResponse'
            Core.<$> (x Core..?> "Valid")
            Core.<*> (x Core..?> "Error")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CheckSchemaVersionValidity

instance Core.NFData CheckSchemaVersionValidity

instance Core.ToHeaders CheckSchemaVersionValidity where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.CheckSchemaVersionValidity" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CheckSchemaVersionValidity where
  toJSON CheckSchemaVersionValidity' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DataFormat" Core..= dataFormat),
            Core.Just
              ("SchemaDefinition" Core..= schemaDefinition)
          ]
      )

instance Core.ToPath CheckSchemaVersionValidity where
  toPath = Core.const "/"

instance Core.ToQuery CheckSchemaVersionValidity where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCheckSchemaVersionValidityResponse' smart constructor.
data CheckSchemaVersionValidityResponse = CheckSchemaVersionValidityResponse'
  { -- | Return true, if the schema is valid and false otherwise.
    valid :: Core.Maybe Core.Bool,
    -- | A validation failure error message.
    error :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CheckSchemaVersionValidityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'valid', 'checkSchemaVersionValidityResponse_valid' - Return true, if the schema is valid and false otherwise.
--
-- 'error', 'checkSchemaVersionValidityResponse_error' - A validation failure error message.
--
-- 'httpStatus', 'checkSchemaVersionValidityResponse_httpStatus' - The response's http status code.
newCheckSchemaVersionValidityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CheckSchemaVersionValidityResponse
newCheckSchemaVersionValidityResponse pHttpStatus_ =
  CheckSchemaVersionValidityResponse'
    { valid =
        Core.Nothing,
      error = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Return true, if the schema is valid and false otherwise.
checkSchemaVersionValidityResponse_valid :: Lens.Lens' CheckSchemaVersionValidityResponse (Core.Maybe Core.Bool)
checkSchemaVersionValidityResponse_valid = Lens.lens (\CheckSchemaVersionValidityResponse' {valid} -> valid) (\s@CheckSchemaVersionValidityResponse' {} a -> s {valid = a} :: CheckSchemaVersionValidityResponse)

-- | A validation failure error message.
checkSchemaVersionValidityResponse_error :: Lens.Lens' CheckSchemaVersionValidityResponse (Core.Maybe Core.Text)
checkSchemaVersionValidityResponse_error = Lens.lens (\CheckSchemaVersionValidityResponse' {error} -> error) (\s@CheckSchemaVersionValidityResponse' {} a -> s {error = a} :: CheckSchemaVersionValidityResponse)

-- | The response's http status code.
checkSchemaVersionValidityResponse_httpStatus :: Lens.Lens' CheckSchemaVersionValidityResponse Core.Int
checkSchemaVersionValidityResponse_httpStatus = Lens.lens (\CheckSchemaVersionValidityResponse' {httpStatus} -> httpStatus) (\s@CheckSchemaVersionValidityResponse' {} a -> s {httpStatus = a} :: CheckSchemaVersionValidityResponse)

instance
  Core.NFData
    CheckSchemaVersionValidityResponse
