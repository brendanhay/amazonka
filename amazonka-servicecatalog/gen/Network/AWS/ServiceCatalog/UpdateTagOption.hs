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
-- Module      : Network.AWS.ServiceCatalog.UpdateTagOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified TagOption.
module Network.AWS.ServiceCatalog.UpdateTagOption
  ( -- * Creating a Request
    UpdateTagOption (..),
    newUpdateTagOption,

    -- * Request Lenses
    updateTagOption_active,
    updateTagOption_value,
    updateTagOption_id,

    -- * Destructuring the Response
    UpdateTagOptionResponse (..),
    newUpdateTagOptionResponse,

    -- * Response Lenses
    updateTagOptionResponse_tagOptionDetail,
    updateTagOptionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateTagOption' smart constructor.
data UpdateTagOption = UpdateTagOption'
  { -- | The updated active state.
    active :: Core.Maybe Core.Bool,
    -- | The updated value.
    value :: Core.Maybe Core.Text,
    -- | The TagOption identifier.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTagOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'active', 'updateTagOption_active' - The updated active state.
--
-- 'value', 'updateTagOption_value' - The updated value.
--
-- 'id', 'updateTagOption_id' - The TagOption identifier.
newUpdateTagOption ::
  -- | 'id'
  Core.Text ->
  UpdateTagOption
newUpdateTagOption pId_ =
  UpdateTagOption'
    { active = Core.Nothing,
      value = Core.Nothing,
      id = pId_
    }

-- | The updated active state.
updateTagOption_active :: Lens.Lens' UpdateTagOption (Core.Maybe Core.Bool)
updateTagOption_active = Lens.lens (\UpdateTagOption' {active} -> active) (\s@UpdateTagOption' {} a -> s {active = a} :: UpdateTagOption)

-- | The updated value.
updateTagOption_value :: Lens.Lens' UpdateTagOption (Core.Maybe Core.Text)
updateTagOption_value = Lens.lens (\UpdateTagOption' {value} -> value) (\s@UpdateTagOption' {} a -> s {value = a} :: UpdateTagOption)

-- | The TagOption identifier.
updateTagOption_id :: Lens.Lens' UpdateTagOption Core.Text
updateTagOption_id = Lens.lens (\UpdateTagOption' {id} -> id) (\s@UpdateTagOption' {} a -> s {id = a} :: UpdateTagOption)

instance Core.AWSRequest UpdateTagOption where
  type
    AWSResponse UpdateTagOption =
      UpdateTagOptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTagOptionResponse'
            Core.<$> (x Core..?> "TagOptionDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTagOption

instance Core.NFData UpdateTagOption

instance Core.ToHeaders UpdateTagOption where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdateTagOption" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTagOption where
  toJSON UpdateTagOption' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Active" Core..=) Core.<$> active,
            ("Value" Core..=) Core.<$> value,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath UpdateTagOption where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTagOption where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateTagOptionResponse' smart constructor.
data UpdateTagOptionResponse = UpdateTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Core.Maybe TagOptionDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTagOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagOptionDetail', 'updateTagOptionResponse_tagOptionDetail' - Information about the TagOption.
--
-- 'httpStatus', 'updateTagOptionResponse_httpStatus' - The response's http status code.
newUpdateTagOptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTagOptionResponse
newUpdateTagOptionResponse pHttpStatus_ =
  UpdateTagOptionResponse'
    { tagOptionDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the TagOption.
updateTagOptionResponse_tagOptionDetail :: Lens.Lens' UpdateTagOptionResponse (Core.Maybe TagOptionDetail)
updateTagOptionResponse_tagOptionDetail = Lens.lens (\UpdateTagOptionResponse' {tagOptionDetail} -> tagOptionDetail) (\s@UpdateTagOptionResponse' {} a -> s {tagOptionDetail = a} :: UpdateTagOptionResponse)

-- | The response's http status code.
updateTagOptionResponse_httpStatus :: Lens.Lens' UpdateTagOptionResponse Core.Int
updateTagOptionResponse_httpStatus = Lens.lens (\UpdateTagOptionResponse' {httpStatus} -> httpStatus) (\s@UpdateTagOptionResponse' {} a -> s {httpStatus = a} :: UpdateTagOptionResponse)

instance Core.NFData UpdateTagOptionResponse
