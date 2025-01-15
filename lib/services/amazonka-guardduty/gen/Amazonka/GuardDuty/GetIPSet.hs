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
-- Module      : Amazonka.GuardDuty.GetIPSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the IPSet specified by the @ipSetId@.
module Amazonka.GuardDuty.GetIPSet
  ( -- * Creating a Request
    GetIPSet (..),
    newGetIPSet,

    -- * Request Lenses
    getIPSet_detectorId,
    getIPSet_ipSetId,

    -- * Destructuring the Response
    GetIPSetResponse (..),
    newGetIPSetResponse,

    -- * Response Lenses
    getIPSetResponse_tags,
    getIPSetResponse_httpStatus,
    getIPSetResponse_name,
    getIPSetResponse_format,
    getIPSetResponse_location,
    getIPSetResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIPSet' smart constructor.
data GetIPSet = GetIPSet'
  { -- | The unique ID of the detector that the IPSet is associated with.
    detectorId :: Prelude.Text,
    -- | The unique ID of the IPSet to retrieve.
    ipSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getIPSet_detectorId' - The unique ID of the detector that the IPSet is associated with.
--
-- 'ipSetId', 'getIPSet_ipSetId' - The unique ID of the IPSet to retrieve.
newGetIPSet ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'ipSetId'
  Prelude.Text ->
  GetIPSet
newGetIPSet pDetectorId_ pIpSetId_ =
  GetIPSet'
    { detectorId = pDetectorId_,
      ipSetId = pIpSetId_
    }

-- | The unique ID of the detector that the IPSet is associated with.
getIPSet_detectorId :: Lens.Lens' GetIPSet Prelude.Text
getIPSet_detectorId = Lens.lens (\GetIPSet' {detectorId} -> detectorId) (\s@GetIPSet' {} a -> s {detectorId = a} :: GetIPSet)

-- | The unique ID of the IPSet to retrieve.
getIPSet_ipSetId :: Lens.Lens' GetIPSet Prelude.Text
getIPSet_ipSetId = Lens.lens (\GetIPSet' {ipSetId} -> ipSetId) (\s@GetIPSet' {} a -> s {ipSetId = a} :: GetIPSet)

instance Core.AWSRequest GetIPSet where
  type AWSResponse GetIPSet = GetIPSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIPSetResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "format")
            Prelude.<*> (x Data..:> "location")
            Prelude.<*> (x Data..:> "status")
      )

instance Prelude.Hashable GetIPSet where
  hashWithSalt _salt GetIPSet' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` ipSetId

instance Prelude.NFData GetIPSet where
  rnf GetIPSet' {..} =
    Prelude.rnf detectorId `Prelude.seq`
      Prelude.rnf ipSetId

instance Data.ToHeaders GetIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIPSet where
  toPath GetIPSet' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/ipset/",
        Data.toBS ipSetId
      ]

instance Data.ToQuery GetIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { -- | The tags of the IPSet resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The user-friendly name for the IPSet.
    name :: Prelude.Text,
    -- | The format of the file that contains the IPSet.
    format :: IpSetFormat,
    -- | The URI of the file that contains the IPSet.
    location :: Prelude.Text,
    -- | The status of IPSet file that was uploaded.
    status :: IpSetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getIPSetResponse_tags' - The tags of the IPSet resource.
--
-- 'httpStatus', 'getIPSetResponse_httpStatus' - The response's http status code.
--
-- 'name', 'getIPSetResponse_name' - The user-friendly name for the IPSet.
--
-- 'format', 'getIPSetResponse_format' - The format of the file that contains the IPSet.
--
-- 'location', 'getIPSetResponse_location' - The URI of the file that contains the IPSet.
--
-- 'status', 'getIPSetResponse_status' - The status of IPSet file that was uploaded.
newGetIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'format'
  IpSetFormat ->
  -- | 'location'
  Prelude.Text ->
  -- | 'status'
  IpSetStatus ->
  GetIPSetResponse
newGetIPSetResponse
  pHttpStatus_
  pName_
  pFormat_
  pLocation_
  pStatus_ =
    GetIPSetResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_,
        format = pFormat_,
        location = pLocation_,
        status = pStatus_
      }

-- | The tags of the IPSet resource.
getIPSetResponse_tags :: Lens.Lens' GetIPSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIPSetResponse_tags = Lens.lens (\GetIPSetResponse' {tags} -> tags) (\s@GetIPSetResponse' {} a -> s {tags = a} :: GetIPSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getIPSetResponse_httpStatus :: Lens.Lens' GetIPSetResponse Prelude.Int
getIPSetResponse_httpStatus = Lens.lens (\GetIPSetResponse' {httpStatus} -> httpStatus) (\s@GetIPSetResponse' {} a -> s {httpStatus = a} :: GetIPSetResponse)

-- | The user-friendly name for the IPSet.
getIPSetResponse_name :: Lens.Lens' GetIPSetResponse Prelude.Text
getIPSetResponse_name = Lens.lens (\GetIPSetResponse' {name} -> name) (\s@GetIPSetResponse' {} a -> s {name = a} :: GetIPSetResponse)

-- | The format of the file that contains the IPSet.
getIPSetResponse_format :: Lens.Lens' GetIPSetResponse IpSetFormat
getIPSetResponse_format = Lens.lens (\GetIPSetResponse' {format} -> format) (\s@GetIPSetResponse' {} a -> s {format = a} :: GetIPSetResponse)

-- | The URI of the file that contains the IPSet.
getIPSetResponse_location :: Lens.Lens' GetIPSetResponse Prelude.Text
getIPSetResponse_location = Lens.lens (\GetIPSetResponse' {location} -> location) (\s@GetIPSetResponse' {} a -> s {location = a} :: GetIPSetResponse)

-- | The status of IPSet file that was uploaded.
getIPSetResponse_status :: Lens.Lens' GetIPSetResponse IpSetStatus
getIPSetResponse_status = Lens.lens (\GetIPSetResponse' {status} -> status) (\s@GetIPSetResponse' {} a -> s {status = a} :: GetIPSetResponse)

instance Prelude.NFData GetIPSetResponse where
  rnf GetIPSetResponse' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf format `Prelude.seq`
            Prelude.rnf location `Prelude.seq`
              Prelude.rnf status
