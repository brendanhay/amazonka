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
-- Module      : Amazonka.MigrationHubStrategy.GetApplicationComponentDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about an application component.
module Amazonka.MigrationHubStrategy.GetApplicationComponentDetails
  ( -- * Creating a Request
    GetApplicationComponentDetails (..),
    newGetApplicationComponentDetails,

    -- * Request Lenses
    getApplicationComponentDetails_applicationComponentId,

    -- * Destructuring the Response
    GetApplicationComponentDetailsResponse (..),
    newGetApplicationComponentDetailsResponse,

    -- * Response Lenses
    getApplicationComponentDetailsResponse_applicationComponentDetail,
    getApplicationComponentDetailsResponse_associatedApplications,
    getApplicationComponentDetailsResponse_associatedServerIds,
    getApplicationComponentDetailsResponse_moreApplicationResource,
    getApplicationComponentDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApplicationComponentDetails' smart constructor.
data GetApplicationComponentDetails = GetApplicationComponentDetails'
  { -- | The ID of the application component. The ID is unique within an AWS
    -- account.
    applicationComponentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationComponentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationComponentId', 'getApplicationComponentDetails_applicationComponentId' - The ID of the application component. The ID is unique within an AWS
-- account.
newGetApplicationComponentDetails ::
  -- | 'applicationComponentId'
  Prelude.Text ->
  GetApplicationComponentDetails
newGetApplicationComponentDetails
  pApplicationComponentId_ =
    GetApplicationComponentDetails'
      { applicationComponentId =
          pApplicationComponentId_
      }

-- | The ID of the application component. The ID is unique within an AWS
-- account.
getApplicationComponentDetails_applicationComponentId :: Lens.Lens' GetApplicationComponentDetails Prelude.Text
getApplicationComponentDetails_applicationComponentId = Lens.lens (\GetApplicationComponentDetails' {applicationComponentId} -> applicationComponentId) (\s@GetApplicationComponentDetails' {} a -> s {applicationComponentId = a} :: GetApplicationComponentDetails)

instance
  Core.AWSRequest
    GetApplicationComponentDetails
  where
  type
    AWSResponse GetApplicationComponentDetails =
      GetApplicationComponentDetailsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationComponentDetailsResponse'
            Prelude.<$> (x Data..?> "applicationComponentDetail")
            Prelude.<*> ( x
                            Data..?> "associatedApplications"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "associatedServerIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "moreApplicationResource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetApplicationComponentDetails
  where
  hashWithSalt
    _salt
    GetApplicationComponentDetails' {..} =
      _salt `Prelude.hashWithSalt` applicationComponentId

instance
  Prelude.NFData
    GetApplicationComponentDetails
  where
  rnf GetApplicationComponentDetails' {..} =
    Prelude.rnf applicationComponentId

instance
  Data.ToHeaders
    GetApplicationComponentDetails
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApplicationComponentDetails where
  toPath GetApplicationComponentDetails' {..} =
    Prelude.mconcat
      [ "/get-applicationcomponent-details/",
        Data.toBS applicationComponentId
      ]

instance Data.ToQuery GetApplicationComponentDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationComponentDetailsResponse' smart constructor.
data GetApplicationComponentDetailsResponse = GetApplicationComponentDetailsResponse'
  { -- | Detailed information about an application component.
    applicationComponentDetail :: Prelude.Maybe ApplicationComponentDetail,
    -- | The associated application group as defined in AWS Application Discovery
    -- Service.
    associatedApplications :: Prelude.Maybe [AssociatedApplication],
    -- | A list of the IDs of the servers on which the application component is
    -- running.
    associatedServerIds :: Prelude.Maybe [Prelude.Text],
    -- | Set to true if the application component belongs to more than one
    -- application group.
    moreApplicationResource :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationComponentDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationComponentDetail', 'getApplicationComponentDetailsResponse_applicationComponentDetail' - Detailed information about an application component.
--
-- 'associatedApplications', 'getApplicationComponentDetailsResponse_associatedApplications' - The associated application group as defined in AWS Application Discovery
-- Service.
--
-- 'associatedServerIds', 'getApplicationComponentDetailsResponse_associatedServerIds' - A list of the IDs of the servers on which the application component is
-- running.
--
-- 'moreApplicationResource', 'getApplicationComponentDetailsResponse_moreApplicationResource' - Set to true if the application component belongs to more than one
-- application group.
--
-- 'httpStatus', 'getApplicationComponentDetailsResponse_httpStatus' - The response's http status code.
newGetApplicationComponentDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApplicationComponentDetailsResponse
newGetApplicationComponentDetailsResponse
  pHttpStatus_ =
    GetApplicationComponentDetailsResponse'
      { applicationComponentDetail =
          Prelude.Nothing,
        associatedApplications =
          Prelude.Nothing,
        associatedServerIds =
          Prelude.Nothing,
        moreApplicationResource =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Detailed information about an application component.
getApplicationComponentDetailsResponse_applicationComponentDetail :: Lens.Lens' GetApplicationComponentDetailsResponse (Prelude.Maybe ApplicationComponentDetail)
getApplicationComponentDetailsResponse_applicationComponentDetail = Lens.lens (\GetApplicationComponentDetailsResponse' {applicationComponentDetail} -> applicationComponentDetail) (\s@GetApplicationComponentDetailsResponse' {} a -> s {applicationComponentDetail = a} :: GetApplicationComponentDetailsResponse)

-- | The associated application group as defined in AWS Application Discovery
-- Service.
getApplicationComponentDetailsResponse_associatedApplications :: Lens.Lens' GetApplicationComponentDetailsResponse (Prelude.Maybe [AssociatedApplication])
getApplicationComponentDetailsResponse_associatedApplications = Lens.lens (\GetApplicationComponentDetailsResponse' {associatedApplications} -> associatedApplications) (\s@GetApplicationComponentDetailsResponse' {} a -> s {associatedApplications = a} :: GetApplicationComponentDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the IDs of the servers on which the application component is
-- running.
getApplicationComponentDetailsResponse_associatedServerIds :: Lens.Lens' GetApplicationComponentDetailsResponse (Prelude.Maybe [Prelude.Text])
getApplicationComponentDetailsResponse_associatedServerIds = Lens.lens (\GetApplicationComponentDetailsResponse' {associatedServerIds} -> associatedServerIds) (\s@GetApplicationComponentDetailsResponse' {} a -> s {associatedServerIds = a} :: GetApplicationComponentDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Set to true if the application component belongs to more than one
-- application group.
getApplicationComponentDetailsResponse_moreApplicationResource :: Lens.Lens' GetApplicationComponentDetailsResponse (Prelude.Maybe Prelude.Bool)
getApplicationComponentDetailsResponse_moreApplicationResource = Lens.lens (\GetApplicationComponentDetailsResponse' {moreApplicationResource} -> moreApplicationResource) (\s@GetApplicationComponentDetailsResponse' {} a -> s {moreApplicationResource = a} :: GetApplicationComponentDetailsResponse)

-- | The response's http status code.
getApplicationComponentDetailsResponse_httpStatus :: Lens.Lens' GetApplicationComponentDetailsResponse Prelude.Int
getApplicationComponentDetailsResponse_httpStatus = Lens.lens (\GetApplicationComponentDetailsResponse' {httpStatus} -> httpStatus) (\s@GetApplicationComponentDetailsResponse' {} a -> s {httpStatus = a} :: GetApplicationComponentDetailsResponse)

instance
  Prelude.NFData
    GetApplicationComponentDetailsResponse
  where
  rnf GetApplicationComponentDetailsResponse' {..} =
    Prelude.rnf applicationComponentDetail
      `Prelude.seq` Prelude.rnf associatedApplications
      `Prelude.seq` Prelude.rnf associatedServerIds
      `Prelude.seq` Prelude.rnf moreApplicationResource
      `Prelude.seq` Prelude.rnf httpStatus
