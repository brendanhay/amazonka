{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ErrorResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ErrorResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorCode
import Amazonka.MigrationHubReFactorSpaces.Types.ErrorResourceType
import qualified Amazonka.Prelude as Prelude

-- | Error associated with a resource returned for a Get or List resource
-- response.
--
-- /See:/ 'newErrorResponse' smart constructor.
data ErrorResponse = ErrorResponse'
  { -- | The Amazon Web Services account ID of the resource owner.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Additional details about the error.
    additionalDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The error code associated with the error.
    code :: Prelude.Maybe ErrorCode,
    -- | The message associated with the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource.
    resourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The type of resource.
    resourceType :: Prelude.Maybe ErrorResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'errorResponse_accountId' - The Amazon Web Services account ID of the resource owner.
--
-- 'additionalDetails', 'errorResponse_additionalDetails' - Additional details about the error.
--
-- 'code', 'errorResponse_code' - The error code associated with the error.
--
-- 'message', 'errorResponse_message' - The message associated with the error.
--
-- 'resourceIdentifier', 'errorResponse_resourceIdentifier' - The ID of the resource.
--
-- 'resourceType', 'errorResponse_resourceType' - The type of resource.
newErrorResponse ::
  ErrorResponse
newErrorResponse =
  ErrorResponse'
    { accountId = Prelude.Nothing,
      additionalDetails = Prelude.Nothing,
      code = Prelude.Nothing,
      message = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the resource owner.
errorResponse_accountId :: Lens.Lens' ErrorResponse (Prelude.Maybe Prelude.Text)
errorResponse_accountId = Lens.lens (\ErrorResponse' {accountId} -> accountId) (\s@ErrorResponse' {} a -> s {accountId = a} :: ErrorResponse)

-- | Additional details about the error.
errorResponse_additionalDetails :: Lens.Lens' ErrorResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
errorResponse_additionalDetails = Lens.lens (\ErrorResponse' {additionalDetails} -> additionalDetails) (\s@ErrorResponse' {} a -> s {additionalDetails = a} :: ErrorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The error code associated with the error.
errorResponse_code :: Lens.Lens' ErrorResponse (Prelude.Maybe ErrorCode)
errorResponse_code = Lens.lens (\ErrorResponse' {code} -> code) (\s@ErrorResponse' {} a -> s {code = a} :: ErrorResponse)

-- | The message associated with the error.
errorResponse_message :: Lens.Lens' ErrorResponse (Prelude.Maybe Prelude.Text)
errorResponse_message = Lens.lens (\ErrorResponse' {message} -> message) (\s@ErrorResponse' {} a -> s {message = a} :: ErrorResponse)

-- | The ID of the resource.
errorResponse_resourceIdentifier :: Lens.Lens' ErrorResponse (Prelude.Maybe Prelude.Text)
errorResponse_resourceIdentifier = Lens.lens (\ErrorResponse' {resourceIdentifier} -> resourceIdentifier) (\s@ErrorResponse' {} a -> s {resourceIdentifier = a} :: ErrorResponse)

-- | The type of resource.
errorResponse_resourceType :: Lens.Lens' ErrorResponse (Prelude.Maybe ErrorResourceType)
errorResponse_resourceType = Lens.lens (\ErrorResponse' {resourceType} -> resourceType) (\s@ErrorResponse' {} a -> s {resourceType = a} :: ErrorResponse)

instance Data.FromJSON ErrorResponse where
  parseJSON =
    Data.withObject
      "ErrorResponse"
      ( \x ->
          ErrorResponse'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> ( x
                            Data..:? "AdditionalDetails"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "ResourceIdentifier")
            Prelude.<*> (x Data..:? "ResourceType")
      )

instance Prelude.Hashable ErrorResponse where
  hashWithSalt _salt ErrorResponse' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` additionalDetails
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ErrorResponse where
  rnf ErrorResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf additionalDetails
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType
