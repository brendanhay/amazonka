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
-- Module      : Amazonka.BackupGateway.PutHypervisorPropertyMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action sets the property mappings for the specified hypervisor. A
-- hypervisor property mapping displays the relationship of entity
-- properties available from the on-premises hypervisor to the properties
-- available in Amazon Web Services.
module Amazonka.BackupGateway.PutHypervisorPropertyMappings
  ( -- * Creating a Request
    PutHypervisorPropertyMappings (..),
    newPutHypervisorPropertyMappings,

    -- * Request Lenses
    putHypervisorPropertyMappings_hypervisorArn,
    putHypervisorPropertyMappings_iamRoleArn,
    putHypervisorPropertyMappings_vmwareToAwsTagMappings,

    -- * Destructuring the Response
    PutHypervisorPropertyMappingsResponse (..),
    newPutHypervisorPropertyMappingsResponse,

    -- * Response Lenses
    putHypervisorPropertyMappingsResponse_hypervisorArn,
    putHypervisorPropertyMappingsResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutHypervisorPropertyMappings' smart constructor.
data PutHypervisorPropertyMappings = PutHypervisorPropertyMappings'
  { -- | The Amazon Resource Name (ARN) of the hypervisor.
    hypervisorArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    iamRoleArn :: Prelude.Text,
    -- | This action requests the mappings of on-premises VMware tags to the
    -- Amazon Web Services tags.
    vmwareToAwsTagMappings :: [VmwareToAwsTagMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutHypervisorPropertyMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'putHypervisorPropertyMappings_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor.
--
-- 'iamRoleArn', 'putHypervisorPropertyMappings_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role.
--
-- 'vmwareToAwsTagMappings', 'putHypervisorPropertyMappings_vmwareToAwsTagMappings' - This action requests the mappings of on-premises VMware tags to the
-- Amazon Web Services tags.
newPutHypervisorPropertyMappings ::
  -- | 'hypervisorArn'
  Prelude.Text ->
  -- | 'iamRoleArn'
  Prelude.Text ->
  PutHypervisorPropertyMappings
newPutHypervisorPropertyMappings
  pHypervisorArn_
  pIamRoleArn_ =
    PutHypervisorPropertyMappings'
      { hypervisorArn =
          pHypervisorArn_,
        iamRoleArn = pIamRoleArn_,
        vmwareToAwsTagMappings = Prelude.mempty
      }

-- | The Amazon Resource Name (ARN) of the hypervisor.
putHypervisorPropertyMappings_hypervisorArn :: Lens.Lens' PutHypervisorPropertyMappings Prelude.Text
putHypervisorPropertyMappings_hypervisorArn = Lens.lens (\PutHypervisorPropertyMappings' {hypervisorArn} -> hypervisorArn) (\s@PutHypervisorPropertyMappings' {} a -> s {hypervisorArn = a} :: PutHypervisorPropertyMappings)

-- | The Amazon Resource Name (ARN) of the IAM role.
putHypervisorPropertyMappings_iamRoleArn :: Lens.Lens' PutHypervisorPropertyMappings Prelude.Text
putHypervisorPropertyMappings_iamRoleArn = Lens.lens (\PutHypervisorPropertyMappings' {iamRoleArn} -> iamRoleArn) (\s@PutHypervisorPropertyMappings' {} a -> s {iamRoleArn = a} :: PutHypervisorPropertyMappings)

-- | This action requests the mappings of on-premises VMware tags to the
-- Amazon Web Services tags.
putHypervisorPropertyMappings_vmwareToAwsTagMappings :: Lens.Lens' PutHypervisorPropertyMappings [VmwareToAwsTagMapping]
putHypervisorPropertyMappings_vmwareToAwsTagMappings = Lens.lens (\PutHypervisorPropertyMappings' {vmwareToAwsTagMappings} -> vmwareToAwsTagMappings) (\s@PutHypervisorPropertyMappings' {} a -> s {vmwareToAwsTagMappings = a} :: PutHypervisorPropertyMappings) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    PutHypervisorPropertyMappings
  where
  type
    AWSResponse PutHypervisorPropertyMappings =
      PutHypervisorPropertyMappingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutHypervisorPropertyMappingsResponse'
            Prelude.<$> (x Data..?> "HypervisorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutHypervisorPropertyMappings
  where
  hashWithSalt _salt PutHypervisorPropertyMappings' {..} =
    _salt `Prelude.hashWithSalt` hypervisorArn
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` vmwareToAwsTagMappings

instance Prelude.NFData PutHypervisorPropertyMappings where
  rnf PutHypervisorPropertyMappings' {..} =
    Prelude.rnf hypervisorArn
      `Prelude.seq` Prelude.rnf iamRoleArn
      `Prelude.seq` Prelude.rnf vmwareToAwsTagMappings

instance Data.ToHeaders PutHypervisorPropertyMappings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.PutHypervisorPropertyMappings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutHypervisorPropertyMappings where
  toJSON PutHypervisorPropertyMappings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HypervisorArn" Data..= hypervisorArn),
            Prelude.Just ("IamRoleArn" Data..= iamRoleArn),
            Prelude.Just
              ( "VmwareToAwsTagMappings"
                  Data..= vmwareToAwsTagMappings
              )
          ]
      )

instance Data.ToPath PutHypervisorPropertyMappings where
  toPath = Prelude.const "/"

instance Data.ToQuery PutHypervisorPropertyMappings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutHypervisorPropertyMappingsResponse' smart constructor.
data PutHypervisorPropertyMappingsResponse = PutHypervisorPropertyMappingsResponse'
  { -- | The Amazon Resource Name (ARN) of the hypervisor.
    hypervisorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutHypervisorPropertyMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'putHypervisorPropertyMappingsResponse_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor.
--
-- 'httpStatus', 'putHypervisorPropertyMappingsResponse_httpStatus' - The response's http status code.
newPutHypervisorPropertyMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutHypervisorPropertyMappingsResponse
newPutHypervisorPropertyMappingsResponse pHttpStatus_ =
  PutHypervisorPropertyMappingsResponse'
    { hypervisorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the hypervisor.
putHypervisorPropertyMappingsResponse_hypervisorArn :: Lens.Lens' PutHypervisorPropertyMappingsResponse (Prelude.Maybe Prelude.Text)
putHypervisorPropertyMappingsResponse_hypervisorArn = Lens.lens (\PutHypervisorPropertyMappingsResponse' {hypervisorArn} -> hypervisorArn) (\s@PutHypervisorPropertyMappingsResponse' {} a -> s {hypervisorArn = a} :: PutHypervisorPropertyMappingsResponse)

-- | The response's http status code.
putHypervisorPropertyMappingsResponse_httpStatus :: Lens.Lens' PutHypervisorPropertyMappingsResponse Prelude.Int
putHypervisorPropertyMappingsResponse_httpStatus = Lens.lens (\PutHypervisorPropertyMappingsResponse' {httpStatus} -> httpStatus) (\s@PutHypervisorPropertyMappingsResponse' {} a -> s {httpStatus = a} :: PutHypervisorPropertyMappingsResponse)

instance
  Prelude.NFData
    PutHypervisorPropertyMappingsResponse
  where
  rnf PutHypervisorPropertyMappingsResponse' {..} =
    Prelude.rnf hypervisorArn
      `Prelude.seq` Prelude.rnf httpStatus
