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
-- Module      : Amazonka.BackupGateway.GetHypervisorPropertyMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action retrieves the property mappings for the specified
-- hypervisor. A hypervisor property mapping displays the relationship of
-- entity properties available from the on-premises hypervisor to the
-- properties available in Amazon Web Services.
module Amazonka.BackupGateway.GetHypervisorPropertyMappings
  ( -- * Creating a Request
    GetHypervisorPropertyMappings (..),
    newGetHypervisorPropertyMappings,

    -- * Request Lenses
    getHypervisorPropertyMappings_hypervisorArn,

    -- * Destructuring the Response
    GetHypervisorPropertyMappingsResponse (..),
    newGetHypervisorPropertyMappingsResponse,

    -- * Response Lenses
    getHypervisorPropertyMappingsResponse_hypervisorArn,
    getHypervisorPropertyMappingsResponse_iamRoleArn,
    getHypervisorPropertyMappingsResponse_vmwareToAwsTagMappings,
    getHypervisorPropertyMappingsResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHypervisorPropertyMappings' smart constructor.
data GetHypervisorPropertyMappings = GetHypervisorPropertyMappings'
  { -- | The Amazon Resource Name (ARN) of the hypervisor.
    hypervisorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHypervisorPropertyMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'getHypervisorPropertyMappings_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor.
newGetHypervisorPropertyMappings ::
  -- | 'hypervisorArn'
  Prelude.Text ->
  GetHypervisorPropertyMappings
newGetHypervisorPropertyMappings pHypervisorArn_ =
  GetHypervisorPropertyMappings'
    { hypervisorArn =
        pHypervisorArn_
    }

-- | The Amazon Resource Name (ARN) of the hypervisor.
getHypervisorPropertyMappings_hypervisorArn :: Lens.Lens' GetHypervisorPropertyMappings Prelude.Text
getHypervisorPropertyMappings_hypervisorArn = Lens.lens (\GetHypervisorPropertyMappings' {hypervisorArn} -> hypervisorArn) (\s@GetHypervisorPropertyMappings' {} a -> s {hypervisorArn = a} :: GetHypervisorPropertyMappings)

instance
  Core.AWSRequest
    GetHypervisorPropertyMappings
  where
  type
    AWSResponse GetHypervisorPropertyMappings =
      GetHypervisorPropertyMappingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHypervisorPropertyMappingsResponse'
            Prelude.<$> (x Data..?> "HypervisorArn")
            Prelude.<*> (x Data..?> "IamRoleArn")
            Prelude.<*> ( x
                            Data..?> "VmwareToAwsTagMappings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetHypervisorPropertyMappings
  where
  hashWithSalt _salt GetHypervisorPropertyMappings' {..} =
    _salt `Prelude.hashWithSalt` hypervisorArn

instance Prelude.NFData GetHypervisorPropertyMappings where
  rnf GetHypervisorPropertyMappings' {..} =
    Prelude.rnf hypervisorArn

instance Data.ToHeaders GetHypervisorPropertyMappings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.GetHypervisorPropertyMappings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetHypervisorPropertyMappings where
  toJSON GetHypervisorPropertyMappings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HypervisorArn" Data..= hypervisorArn)
          ]
      )

instance Data.ToPath GetHypervisorPropertyMappings where
  toPath = Prelude.const "/"

instance Data.ToQuery GetHypervisorPropertyMappings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetHypervisorPropertyMappingsResponse' smart constructor.
data GetHypervisorPropertyMappingsResponse = GetHypervisorPropertyMappingsResponse'
  { -- | The Amazon Resource Name (ARN) of the hypervisor.
    hypervisorArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | This is a display of the mappings of on-premises VMware tags to the
    -- Amazon Web Services tags.
    vmwareToAwsTagMappings :: Prelude.Maybe [VmwareToAwsTagMapping],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHypervisorPropertyMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'getHypervisorPropertyMappingsResponse_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor.
--
-- 'iamRoleArn', 'getHypervisorPropertyMappingsResponse_iamRoleArn' - The Amazon Resource Name (ARN) of the IAM role.
--
-- 'vmwareToAwsTagMappings', 'getHypervisorPropertyMappingsResponse_vmwareToAwsTagMappings' - This is a display of the mappings of on-premises VMware tags to the
-- Amazon Web Services tags.
--
-- 'httpStatus', 'getHypervisorPropertyMappingsResponse_httpStatus' - The response's http status code.
newGetHypervisorPropertyMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHypervisorPropertyMappingsResponse
newGetHypervisorPropertyMappingsResponse pHttpStatus_ =
  GetHypervisorPropertyMappingsResponse'
    { hypervisorArn =
        Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      vmwareToAwsTagMappings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the hypervisor.
getHypervisorPropertyMappingsResponse_hypervisorArn :: Lens.Lens' GetHypervisorPropertyMappingsResponse (Prelude.Maybe Prelude.Text)
getHypervisorPropertyMappingsResponse_hypervisorArn = Lens.lens (\GetHypervisorPropertyMappingsResponse' {hypervisorArn} -> hypervisorArn) (\s@GetHypervisorPropertyMappingsResponse' {} a -> s {hypervisorArn = a} :: GetHypervisorPropertyMappingsResponse)

-- | The Amazon Resource Name (ARN) of the IAM role.
getHypervisorPropertyMappingsResponse_iamRoleArn :: Lens.Lens' GetHypervisorPropertyMappingsResponse (Prelude.Maybe Prelude.Text)
getHypervisorPropertyMappingsResponse_iamRoleArn = Lens.lens (\GetHypervisorPropertyMappingsResponse' {iamRoleArn} -> iamRoleArn) (\s@GetHypervisorPropertyMappingsResponse' {} a -> s {iamRoleArn = a} :: GetHypervisorPropertyMappingsResponse)

-- | This is a display of the mappings of on-premises VMware tags to the
-- Amazon Web Services tags.
getHypervisorPropertyMappingsResponse_vmwareToAwsTagMappings :: Lens.Lens' GetHypervisorPropertyMappingsResponse (Prelude.Maybe [VmwareToAwsTagMapping])
getHypervisorPropertyMappingsResponse_vmwareToAwsTagMappings = Lens.lens (\GetHypervisorPropertyMappingsResponse' {vmwareToAwsTagMappings} -> vmwareToAwsTagMappings) (\s@GetHypervisorPropertyMappingsResponse' {} a -> s {vmwareToAwsTagMappings = a} :: GetHypervisorPropertyMappingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getHypervisorPropertyMappingsResponse_httpStatus :: Lens.Lens' GetHypervisorPropertyMappingsResponse Prelude.Int
getHypervisorPropertyMappingsResponse_httpStatus = Lens.lens (\GetHypervisorPropertyMappingsResponse' {httpStatus} -> httpStatus) (\s@GetHypervisorPropertyMappingsResponse' {} a -> s {httpStatus = a} :: GetHypervisorPropertyMappingsResponse)

instance
  Prelude.NFData
    GetHypervisorPropertyMappingsResponse
  where
  rnf GetHypervisorPropertyMappingsResponse' {..} =
    Prelude.rnf hypervisorArn `Prelude.seq`
      Prelude.rnf iamRoleArn `Prelude.seq`
        Prelude.rnf vmwareToAwsTagMappings `Prelude.seq`
          Prelude.rnf httpStatus
