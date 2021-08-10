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
-- Module      : Network.AWS.GuardDuty.GetIPSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the IPSet specified by the @ipSetId@.
module Network.AWS.GuardDuty.GetIPSet
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

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIPSetResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> (x Core..:> "format")
            Prelude.<*> (x Core..:> "location")
            Prelude.<*> (x Core..:> "status")
      )

instance Prelude.Hashable GetIPSet

instance Prelude.NFData GetIPSet

instance Core.ToHeaders GetIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetIPSet where
  toPath GetIPSet' {..} =
    Prelude.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/ipset/",
        Core.toBS ipSetId
      ]

instance Core.ToQuery GetIPSet where
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
    -- | The URI of the file that contains the IPSet. For example:
    -- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
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
-- 'location', 'getIPSetResponse_location' - The URI of the file that contains the IPSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
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
getIPSetResponse_tags = Lens.lens (\GetIPSetResponse' {tags} -> tags) (\s@GetIPSetResponse' {} a -> s {tags = a} :: GetIPSetResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getIPSetResponse_httpStatus :: Lens.Lens' GetIPSetResponse Prelude.Int
getIPSetResponse_httpStatus = Lens.lens (\GetIPSetResponse' {httpStatus} -> httpStatus) (\s@GetIPSetResponse' {} a -> s {httpStatus = a} :: GetIPSetResponse)

-- | The user-friendly name for the IPSet.
getIPSetResponse_name :: Lens.Lens' GetIPSetResponse Prelude.Text
getIPSetResponse_name = Lens.lens (\GetIPSetResponse' {name} -> name) (\s@GetIPSetResponse' {} a -> s {name = a} :: GetIPSetResponse)

-- | The format of the file that contains the IPSet.
getIPSetResponse_format :: Lens.Lens' GetIPSetResponse IpSetFormat
getIPSetResponse_format = Lens.lens (\GetIPSetResponse' {format} -> format) (\s@GetIPSetResponse' {} a -> s {format = a} :: GetIPSetResponse)

-- | The URI of the file that contains the IPSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
getIPSetResponse_location :: Lens.Lens' GetIPSetResponse Prelude.Text
getIPSetResponse_location = Lens.lens (\GetIPSetResponse' {location} -> location) (\s@GetIPSetResponse' {} a -> s {location = a} :: GetIPSetResponse)

-- | The status of IPSet file that was uploaded.
getIPSetResponse_status :: Lens.Lens' GetIPSetResponse IpSetStatus
getIPSetResponse_status = Lens.lens (\GetIPSetResponse' {status} -> status) (\s@GetIPSetResponse' {} a -> s {status = a} :: GetIPSetResponse)

instance Prelude.NFData GetIPSetResponse
