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
-- Module      : Amazonka.SageMaker.GetLineageGroupPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The resource policy for the lineage group.
module Amazonka.SageMaker.GetLineageGroupPolicy
  ( -- * Creating a Request
    GetLineageGroupPolicy (..),
    newGetLineageGroupPolicy,

    -- * Request Lenses
    getLineageGroupPolicy_lineageGroupName,

    -- * Destructuring the Response
    GetLineageGroupPolicyResponse (..),
    newGetLineageGroupPolicyResponse,

    -- * Response Lenses
    getLineageGroupPolicyResponse_resourcePolicy,
    getLineageGroupPolicyResponse_lineageGroupArn,
    getLineageGroupPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newGetLineageGroupPolicy' smart constructor.
data GetLineageGroupPolicy = GetLineageGroupPolicy'
  { -- | The name or Amazon Resource Name (ARN) of the lineage group.
    lineageGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLineageGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineageGroupName', 'getLineageGroupPolicy_lineageGroupName' - The name or Amazon Resource Name (ARN) of the lineage group.
newGetLineageGroupPolicy ::
  -- | 'lineageGroupName'
  Prelude.Text ->
  GetLineageGroupPolicy
newGetLineageGroupPolicy pLineageGroupName_ =
  GetLineageGroupPolicy'
    { lineageGroupName =
        pLineageGroupName_
    }

-- | The name or Amazon Resource Name (ARN) of the lineage group.
getLineageGroupPolicy_lineageGroupName :: Lens.Lens' GetLineageGroupPolicy Prelude.Text
getLineageGroupPolicy_lineageGroupName = Lens.lens (\GetLineageGroupPolicy' {lineageGroupName} -> lineageGroupName) (\s@GetLineageGroupPolicy' {} a -> s {lineageGroupName = a} :: GetLineageGroupPolicy)

instance Core.AWSRequest GetLineageGroupPolicy where
  type
    AWSResponse GetLineageGroupPolicy =
      GetLineageGroupPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLineageGroupPolicyResponse'
            Prelude.<$> (x Data..?> "ResourcePolicy")
            Prelude.<*> (x Data..?> "LineageGroupArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLineageGroupPolicy where
  hashWithSalt _salt GetLineageGroupPolicy' {..} =
    _salt `Prelude.hashWithSalt` lineageGroupName

instance Prelude.NFData GetLineageGroupPolicy where
  rnf GetLineageGroupPolicy' {..} =
    Prelude.rnf lineageGroupName

instance Data.ToHeaders GetLineageGroupPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.GetLineageGroupPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLineageGroupPolicy where
  toJSON GetLineageGroupPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LineageGroupName" Data..= lineageGroupName)
          ]
      )

instance Data.ToPath GetLineageGroupPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLineageGroupPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLineageGroupPolicyResponse' smart constructor.
data GetLineageGroupPolicyResponse = GetLineageGroupPolicyResponse'
  { -- | The resource policy that gives access to the lineage group in another
    -- account.
    resourcePolicy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the lineage group.
    lineageGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLineageGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourcePolicy', 'getLineageGroupPolicyResponse_resourcePolicy' - The resource policy that gives access to the lineage group in another
-- account.
--
-- 'lineageGroupArn', 'getLineageGroupPolicyResponse_lineageGroupArn' - The Amazon Resource Name (ARN) of the lineage group.
--
-- 'httpStatus', 'getLineageGroupPolicyResponse_httpStatus' - The response's http status code.
newGetLineageGroupPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLineageGroupPolicyResponse
newGetLineageGroupPolicyResponse pHttpStatus_ =
  GetLineageGroupPolicyResponse'
    { resourcePolicy =
        Prelude.Nothing,
      lineageGroupArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource policy that gives access to the lineage group in another
-- account.
getLineageGroupPolicyResponse_resourcePolicy :: Lens.Lens' GetLineageGroupPolicyResponse (Prelude.Maybe Prelude.Text)
getLineageGroupPolicyResponse_resourcePolicy = Lens.lens (\GetLineageGroupPolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@GetLineageGroupPolicyResponse' {} a -> s {resourcePolicy = a} :: GetLineageGroupPolicyResponse)

-- | The Amazon Resource Name (ARN) of the lineage group.
getLineageGroupPolicyResponse_lineageGroupArn :: Lens.Lens' GetLineageGroupPolicyResponse (Prelude.Maybe Prelude.Text)
getLineageGroupPolicyResponse_lineageGroupArn = Lens.lens (\GetLineageGroupPolicyResponse' {lineageGroupArn} -> lineageGroupArn) (\s@GetLineageGroupPolicyResponse' {} a -> s {lineageGroupArn = a} :: GetLineageGroupPolicyResponse)

-- | The response's http status code.
getLineageGroupPolicyResponse_httpStatus :: Lens.Lens' GetLineageGroupPolicyResponse Prelude.Int
getLineageGroupPolicyResponse_httpStatus = Lens.lens (\GetLineageGroupPolicyResponse' {httpStatus} -> httpStatus) (\s@GetLineageGroupPolicyResponse' {} a -> s {httpStatus = a} :: GetLineageGroupPolicyResponse)

instance Prelude.NFData GetLineageGroupPolicyResponse where
  rnf GetLineageGroupPolicyResponse' {..} =
    Prelude.rnf resourcePolicy
      `Prelude.seq` Prelude.rnf lineageGroupArn
      `Prelude.seq` Prelude.rnf httpStatus
