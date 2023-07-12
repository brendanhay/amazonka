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
-- Module      : Amazonka.OpenSearch.Types.VpcEndpointError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.VpcEndpointError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.VpcEndpointErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Error information when attempting to describe an Amazon OpenSearch
-- Service-managed VPC endpoint.
--
-- /See:/ 'newVpcEndpointError' smart constructor.
data VpcEndpointError = VpcEndpointError'
  { -- | The code associated with the error.
    errorCode :: Prelude.Maybe VpcEndpointErrorCode,
    -- | A message describing the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the endpoint.
    vpcEndpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpointError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'vpcEndpointError_errorCode' - The code associated with the error.
--
-- 'errorMessage', 'vpcEndpointError_errorMessage' - A message describing the error.
--
-- 'vpcEndpointId', 'vpcEndpointError_vpcEndpointId' - The unique identifier of the endpoint.
newVpcEndpointError ::
  VpcEndpointError
newVpcEndpointError =
  VpcEndpointError'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      vpcEndpointId = Prelude.Nothing
    }

-- | The code associated with the error.
vpcEndpointError_errorCode :: Lens.Lens' VpcEndpointError (Prelude.Maybe VpcEndpointErrorCode)
vpcEndpointError_errorCode = Lens.lens (\VpcEndpointError' {errorCode} -> errorCode) (\s@VpcEndpointError' {} a -> s {errorCode = a} :: VpcEndpointError)

-- | A message describing the error.
vpcEndpointError_errorMessage :: Lens.Lens' VpcEndpointError (Prelude.Maybe Prelude.Text)
vpcEndpointError_errorMessage = Lens.lens (\VpcEndpointError' {errorMessage} -> errorMessage) (\s@VpcEndpointError' {} a -> s {errorMessage = a} :: VpcEndpointError)

-- | The unique identifier of the endpoint.
vpcEndpointError_vpcEndpointId :: Lens.Lens' VpcEndpointError (Prelude.Maybe Prelude.Text)
vpcEndpointError_vpcEndpointId = Lens.lens (\VpcEndpointError' {vpcEndpointId} -> vpcEndpointId) (\s@VpcEndpointError' {} a -> s {vpcEndpointId = a} :: VpcEndpointError)

instance Data.FromJSON VpcEndpointError where
  parseJSON =
    Data.withObject
      "VpcEndpointError"
      ( \x ->
          VpcEndpointError'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "VpcEndpointId")
      )

instance Prelude.Hashable VpcEndpointError where
  hashWithSalt _salt VpcEndpointError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` vpcEndpointId

instance Prelude.NFData VpcEndpointError where
  rnf VpcEndpointError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf vpcEndpointId
