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
-- Module      : Amazonka.HealthLake.DescribeFHIRDatastore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with the FHIR Data Store, including the
-- Data Store ID, Data Store ARN, Data Store name, Data Store status,
-- created at, Data Store type version, and Data Store endpoint.
module Amazonka.HealthLake.DescribeFHIRDatastore
  ( -- * Creating a Request
    DescribeFHIRDatastore (..),
    newDescribeFHIRDatastore,

    -- * Request Lenses
    describeFHIRDatastore_datastoreId,

    -- * Destructuring the Response
    DescribeFHIRDatastoreResponse (..),
    newDescribeFHIRDatastoreResponse,

    -- * Response Lenses
    describeFHIRDatastoreResponse_httpStatus,
    describeFHIRDatastoreResponse_datastoreProperties,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.HealthLake.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFHIRDatastore' smart constructor.
data DescribeFHIRDatastore = DescribeFHIRDatastore'
  { -- | The AWS-generated Data Store id. This is part of the
    -- ‘CreateFHIRDatastore’ output.
    datastoreId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFHIRDatastore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datastoreId', 'describeFHIRDatastore_datastoreId' - The AWS-generated Data Store id. This is part of the
-- ‘CreateFHIRDatastore’ output.
newDescribeFHIRDatastore ::
  DescribeFHIRDatastore
newDescribeFHIRDatastore =
  DescribeFHIRDatastore'
    { datastoreId =
        Prelude.Nothing
    }

-- | The AWS-generated Data Store id. This is part of the
-- ‘CreateFHIRDatastore’ output.
describeFHIRDatastore_datastoreId :: Lens.Lens' DescribeFHIRDatastore (Prelude.Maybe Prelude.Text)
describeFHIRDatastore_datastoreId = Lens.lens (\DescribeFHIRDatastore' {datastoreId} -> datastoreId) (\s@DescribeFHIRDatastore' {} a -> s {datastoreId = a} :: DescribeFHIRDatastore)

instance Core.AWSRequest DescribeFHIRDatastore where
  type
    AWSResponse DescribeFHIRDatastore =
      DescribeFHIRDatastoreResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFHIRDatastoreResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DatastoreProperties")
      )

instance Prelude.Hashable DescribeFHIRDatastore where
  hashWithSalt _salt DescribeFHIRDatastore' {..} =
    _salt `Prelude.hashWithSalt` datastoreId

instance Prelude.NFData DescribeFHIRDatastore where
  rnf DescribeFHIRDatastore' {..} =
    Prelude.rnf datastoreId

instance Core.ToHeaders DescribeFHIRDatastore where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "HealthLake.DescribeFHIRDatastore" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFHIRDatastore where
  toJSON DescribeFHIRDatastore' {..} =
    Core.object
      ( Prelude.catMaybes
          [("DatastoreId" Core..=) Prelude.<$> datastoreId]
      )

instance Core.ToPath DescribeFHIRDatastore where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFHIRDatastore where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFHIRDatastoreResponse' smart constructor.
data DescribeFHIRDatastoreResponse = DescribeFHIRDatastoreResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | All properties associated with a Data Store, including the Data Store
    -- ID, Data Store ARN, Data Store name, Data Store status, created at, Data
    -- Store type version, and Data Store endpoint.
    datastoreProperties :: DatastoreProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFHIRDatastoreResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeFHIRDatastoreResponse_httpStatus' - The response's http status code.
--
-- 'datastoreProperties', 'describeFHIRDatastoreResponse_datastoreProperties' - All properties associated with a Data Store, including the Data Store
-- ID, Data Store ARN, Data Store name, Data Store status, created at, Data
-- Store type version, and Data Store endpoint.
newDescribeFHIRDatastoreResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'datastoreProperties'
  DatastoreProperties ->
  DescribeFHIRDatastoreResponse
newDescribeFHIRDatastoreResponse
  pHttpStatus_
  pDatastoreProperties_ =
    DescribeFHIRDatastoreResponse'
      { httpStatus =
          pHttpStatus_,
        datastoreProperties = pDatastoreProperties_
      }

-- | The response's http status code.
describeFHIRDatastoreResponse_httpStatus :: Lens.Lens' DescribeFHIRDatastoreResponse Prelude.Int
describeFHIRDatastoreResponse_httpStatus = Lens.lens (\DescribeFHIRDatastoreResponse' {httpStatus} -> httpStatus) (\s@DescribeFHIRDatastoreResponse' {} a -> s {httpStatus = a} :: DescribeFHIRDatastoreResponse)

-- | All properties associated with a Data Store, including the Data Store
-- ID, Data Store ARN, Data Store name, Data Store status, created at, Data
-- Store type version, and Data Store endpoint.
describeFHIRDatastoreResponse_datastoreProperties :: Lens.Lens' DescribeFHIRDatastoreResponse DatastoreProperties
describeFHIRDatastoreResponse_datastoreProperties = Lens.lens (\DescribeFHIRDatastoreResponse' {datastoreProperties} -> datastoreProperties) (\s@DescribeFHIRDatastoreResponse' {} a -> s {datastoreProperties = a} :: DescribeFHIRDatastoreResponse)

instance Prelude.NFData DescribeFHIRDatastoreResponse where
  rnf DescribeFHIRDatastoreResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf datastoreProperties
