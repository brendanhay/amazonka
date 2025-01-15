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
-- Module      : Amazonka.FIS.GetTargetResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified resource type.
module Amazonka.FIS.GetTargetResourceType
  ( -- * Creating a Request
    GetTargetResourceType (..),
    newGetTargetResourceType,

    -- * Request Lenses
    getTargetResourceType_resourceType,

    -- * Destructuring the Response
    GetTargetResourceTypeResponse (..),
    newGetTargetResourceTypeResponse,

    -- * Response Lenses
    getTargetResourceTypeResponse_targetResourceType,
    getTargetResourceTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTargetResourceType' smart constructor.
data GetTargetResourceType = GetTargetResourceType'
  { -- | The resource type.
    resourceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTargetResourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'getTargetResourceType_resourceType' - The resource type.
newGetTargetResourceType ::
  -- | 'resourceType'
  Prelude.Text ->
  GetTargetResourceType
newGetTargetResourceType pResourceType_ =
  GetTargetResourceType'
    { resourceType =
        pResourceType_
    }

-- | The resource type.
getTargetResourceType_resourceType :: Lens.Lens' GetTargetResourceType Prelude.Text
getTargetResourceType_resourceType = Lens.lens (\GetTargetResourceType' {resourceType} -> resourceType) (\s@GetTargetResourceType' {} a -> s {resourceType = a} :: GetTargetResourceType)

instance Core.AWSRequest GetTargetResourceType where
  type
    AWSResponse GetTargetResourceType =
      GetTargetResourceTypeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTargetResourceTypeResponse'
            Prelude.<$> (x Data..?> "targetResourceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTargetResourceType where
  hashWithSalt _salt GetTargetResourceType' {..} =
    _salt `Prelude.hashWithSalt` resourceType

instance Prelude.NFData GetTargetResourceType where
  rnf GetTargetResourceType' {..} =
    Prelude.rnf resourceType

instance Data.ToHeaders GetTargetResourceType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTargetResourceType where
  toPath GetTargetResourceType' {..} =
    Prelude.mconcat
      ["/targetResourceTypes/", Data.toBS resourceType]

instance Data.ToQuery GetTargetResourceType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTargetResourceTypeResponse' smart constructor.
data GetTargetResourceTypeResponse = GetTargetResourceTypeResponse'
  { -- | Information about the resource type.
    targetResourceType :: Prelude.Maybe TargetResourceType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTargetResourceTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetResourceType', 'getTargetResourceTypeResponse_targetResourceType' - Information about the resource type.
--
-- 'httpStatus', 'getTargetResourceTypeResponse_httpStatus' - The response's http status code.
newGetTargetResourceTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTargetResourceTypeResponse
newGetTargetResourceTypeResponse pHttpStatus_ =
  GetTargetResourceTypeResponse'
    { targetResourceType =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the resource type.
getTargetResourceTypeResponse_targetResourceType :: Lens.Lens' GetTargetResourceTypeResponse (Prelude.Maybe TargetResourceType)
getTargetResourceTypeResponse_targetResourceType = Lens.lens (\GetTargetResourceTypeResponse' {targetResourceType} -> targetResourceType) (\s@GetTargetResourceTypeResponse' {} a -> s {targetResourceType = a} :: GetTargetResourceTypeResponse)

-- | The response's http status code.
getTargetResourceTypeResponse_httpStatus :: Lens.Lens' GetTargetResourceTypeResponse Prelude.Int
getTargetResourceTypeResponse_httpStatus = Lens.lens (\GetTargetResourceTypeResponse' {httpStatus} -> httpStatus) (\s@GetTargetResourceTypeResponse' {} a -> s {httpStatus = a} :: GetTargetResourceTypeResponse)

instance Prelude.NFData GetTargetResourceTypeResponse where
  rnf GetTargetResourceTypeResponse' {..} =
    Prelude.rnf targetResourceType `Prelude.seq`
      Prelude.rnf httpStatus
