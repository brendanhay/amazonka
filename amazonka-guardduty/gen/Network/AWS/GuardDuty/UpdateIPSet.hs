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
-- Module      : Network.AWS.GuardDuty.UpdateIPSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the IPSet specified by the IPSet ID.
module Network.AWS.GuardDuty.UpdateIPSet
  ( -- * Creating a Request
    UpdateIPSet (..),
    newUpdateIPSet,

    -- * Request Lenses
    updateIPSet_activate,
    updateIPSet_name,
    updateIPSet_location,
    updateIPSet_detectorId,
    updateIPSet_ipSetId,

    -- * Destructuring the Response
    UpdateIPSetResponse (..),
    newUpdateIPSetResponse,

    -- * Response Lenses
    updateIPSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { -- | The updated Boolean value that specifies whether the IPSet is active or
    -- not.
    activate :: Core.Maybe Core.Bool,
    -- | The unique ID that specifies the IPSet that you want to update.
    name :: Core.Maybe Core.Text,
    -- | The updated URI of the file that contains the IPSet. For example:
    -- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
    location :: Core.Maybe Core.Text,
    -- | The detectorID that specifies the GuardDuty service whose IPSet you want
    -- to update.
    detectorId :: Core.Text,
    -- | The unique ID that specifies the IPSet that you want to update.
    ipSetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activate', 'updateIPSet_activate' - The updated Boolean value that specifies whether the IPSet is active or
-- not.
--
-- 'name', 'updateIPSet_name' - The unique ID that specifies the IPSet that you want to update.
--
-- 'location', 'updateIPSet_location' - The updated URI of the file that contains the IPSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
--
-- 'detectorId', 'updateIPSet_detectorId' - The detectorID that specifies the GuardDuty service whose IPSet you want
-- to update.
--
-- 'ipSetId', 'updateIPSet_ipSetId' - The unique ID that specifies the IPSet that you want to update.
newUpdateIPSet ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'ipSetId'
  Core.Text ->
  UpdateIPSet
newUpdateIPSet pDetectorId_ pIpSetId_ =
  UpdateIPSet'
    { activate = Core.Nothing,
      name = Core.Nothing,
      location = Core.Nothing,
      detectorId = pDetectorId_,
      ipSetId = pIpSetId_
    }

-- | The updated Boolean value that specifies whether the IPSet is active or
-- not.
updateIPSet_activate :: Lens.Lens' UpdateIPSet (Core.Maybe Core.Bool)
updateIPSet_activate = Lens.lens (\UpdateIPSet' {activate} -> activate) (\s@UpdateIPSet' {} a -> s {activate = a} :: UpdateIPSet)

-- | The unique ID that specifies the IPSet that you want to update.
updateIPSet_name :: Lens.Lens' UpdateIPSet (Core.Maybe Core.Text)
updateIPSet_name = Lens.lens (\UpdateIPSet' {name} -> name) (\s@UpdateIPSet' {} a -> s {name = a} :: UpdateIPSet)

-- | The updated URI of the file that contains the IPSet. For example:
-- https:\/\/s3.us-west-2.amazonaws.com\/my-bucket\/my-object-key.
updateIPSet_location :: Lens.Lens' UpdateIPSet (Core.Maybe Core.Text)
updateIPSet_location = Lens.lens (\UpdateIPSet' {location} -> location) (\s@UpdateIPSet' {} a -> s {location = a} :: UpdateIPSet)

-- | The detectorID that specifies the GuardDuty service whose IPSet you want
-- to update.
updateIPSet_detectorId :: Lens.Lens' UpdateIPSet Core.Text
updateIPSet_detectorId = Lens.lens (\UpdateIPSet' {detectorId} -> detectorId) (\s@UpdateIPSet' {} a -> s {detectorId = a} :: UpdateIPSet)

-- | The unique ID that specifies the IPSet that you want to update.
updateIPSet_ipSetId :: Lens.Lens' UpdateIPSet Core.Text
updateIPSet_ipSetId = Lens.lens (\UpdateIPSet' {ipSetId} -> ipSetId) (\s@UpdateIPSet' {} a -> s {ipSetId = a} :: UpdateIPSet)

instance Core.AWSRequest UpdateIPSet where
  type AWSResponse UpdateIPSet = UpdateIPSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateIPSetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateIPSet

instance Core.NFData UpdateIPSet

instance Core.ToHeaders UpdateIPSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateIPSet where
  toJSON UpdateIPSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("activate" Core..=) Core.<$> activate,
            ("name" Core..=) Core.<$> name,
            ("location" Core..=) Core.<$> location
          ]
      )

instance Core.ToPath UpdateIPSet where
  toPath UpdateIPSet' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/ipset/",
        Core.toBS ipSetId
      ]

instance Core.ToQuery UpdateIPSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateIPSetResponse' smart constructor.
data UpdateIPSetResponse = UpdateIPSetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateIPSetResponse_httpStatus' - The response's http status code.
newUpdateIPSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateIPSetResponse
newUpdateIPSetResponse pHttpStatus_ =
  UpdateIPSetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateIPSetResponse_httpStatus :: Lens.Lens' UpdateIPSetResponse Core.Int
updateIPSetResponse_httpStatus = Lens.lens (\UpdateIPSetResponse' {httpStatus} -> httpStatus) (\s@UpdateIPSetResponse' {} a -> s {httpStatus = a} :: UpdateIPSetResponse)

instance Core.NFData UpdateIPSetResponse
