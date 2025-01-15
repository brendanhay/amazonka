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
-- Module      : Amazonka.OpenSearchServerless.Types.VpcEndpointErrorDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types.VpcEndpointErrorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Error information for a failed @BatchGetVpcEndpoint@ request.
--
-- /See:/ 'newVpcEndpointErrorDetail' smart constructor.
data VpcEndpointErrorDetail = VpcEndpointErrorDetail'
  { -- | The error code for the failed request.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | An error message describing the reason for the failure.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the VPC endpoint.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcEndpointErrorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'vpcEndpointErrorDetail_errorCode' - The error code for the failed request.
--
-- 'errorMessage', 'vpcEndpointErrorDetail_errorMessage' - An error message describing the reason for the failure.
--
-- 'id', 'vpcEndpointErrorDetail_id' - The unique identifier of the VPC endpoint.
newVpcEndpointErrorDetail ::
  VpcEndpointErrorDetail
newVpcEndpointErrorDetail =
  VpcEndpointErrorDetail'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The error code for the failed request.
vpcEndpointErrorDetail_errorCode :: Lens.Lens' VpcEndpointErrorDetail (Prelude.Maybe Prelude.Text)
vpcEndpointErrorDetail_errorCode = Lens.lens (\VpcEndpointErrorDetail' {errorCode} -> errorCode) (\s@VpcEndpointErrorDetail' {} a -> s {errorCode = a} :: VpcEndpointErrorDetail)

-- | An error message describing the reason for the failure.
vpcEndpointErrorDetail_errorMessage :: Lens.Lens' VpcEndpointErrorDetail (Prelude.Maybe Prelude.Text)
vpcEndpointErrorDetail_errorMessage = Lens.lens (\VpcEndpointErrorDetail' {errorMessage} -> errorMessage) (\s@VpcEndpointErrorDetail' {} a -> s {errorMessage = a} :: VpcEndpointErrorDetail)

-- | The unique identifier of the VPC endpoint.
vpcEndpointErrorDetail_id :: Lens.Lens' VpcEndpointErrorDetail (Prelude.Maybe Prelude.Text)
vpcEndpointErrorDetail_id = Lens.lens (\VpcEndpointErrorDetail' {id} -> id) (\s@VpcEndpointErrorDetail' {} a -> s {id = a} :: VpcEndpointErrorDetail)

instance Data.FromJSON VpcEndpointErrorDetail where
  parseJSON =
    Data.withObject
      "VpcEndpointErrorDetail"
      ( \x ->
          VpcEndpointErrorDetail'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable VpcEndpointErrorDetail where
  hashWithSalt _salt VpcEndpointErrorDetail' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` id

instance Prelude.NFData VpcEndpointErrorDetail where
  rnf VpcEndpointErrorDetail' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf id
