{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateTagOption' smart constructor.
data UpdateTagOption = UpdateTagOption'
  { -- | The updated active state.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The updated value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The TagOption identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateTagOption
newUpdateTagOption pId_ =
  UpdateTagOption'
    { active = Prelude.Nothing,
      value = Prelude.Nothing,
      id = pId_
    }

-- | The updated active state.
updateTagOption_active :: Lens.Lens' UpdateTagOption (Prelude.Maybe Prelude.Bool)
updateTagOption_active = Lens.lens (\UpdateTagOption' {active} -> active) (\s@UpdateTagOption' {} a -> s {active = a} :: UpdateTagOption)

-- | The updated value.
updateTagOption_value :: Lens.Lens' UpdateTagOption (Prelude.Maybe Prelude.Text)
updateTagOption_value = Lens.lens (\UpdateTagOption' {value} -> value) (\s@UpdateTagOption' {} a -> s {value = a} :: UpdateTagOption)

-- | The TagOption identifier.
updateTagOption_id :: Lens.Lens' UpdateTagOption Prelude.Text
updateTagOption_id = Lens.lens (\UpdateTagOption' {id} -> id) (\s@UpdateTagOption' {} a -> s {id = a} :: UpdateTagOption)

instance Prelude.AWSRequest UpdateTagOption where
  type Rs UpdateTagOption = UpdateTagOptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTagOptionResponse'
            Prelude.<$> (x Prelude..?> "TagOptionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTagOption

instance Prelude.NFData UpdateTagOption

instance Prelude.ToHeaders UpdateTagOption where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.UpdateTagOption" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateTagOption where
  toJSON UpdateTagOption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Active" Prelude..=) Prelude.<$> active,
            ("Value" Prelude..=) Prelude.<$> value,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath UpdateTagOption where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateTagOption where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTagOptionResponse' smart constructor.
data UpdateTagOptionResponse = UpdateTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Prelude.Maybe TagOptionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateTagOptionResponse
newUpdateTagOptionResponse pHttpStatus_ =
  UpdateTagOptionResponse'
    { tagOptionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the TagOption.
updateTagOptionResponse_tagOptionDetail :: Lens.Lens' UpdateTagOptionResponse (Prelude.Maybe TagOptionDetail)
updateTagOptionResponse_tagOptionDetail = Lens.lens (\UpdateTagOptionResponse' {tagOptionDetail} -> tagOptionDetail) (\s@UpdateTagOptionResponse' {} a -> s {tagOptionDetail = a} :: UpdateTagOptionResponse)

-- | The response's http status code.
updateTagOptionResponse_httpStatus :: Lens.Lens' UpdateTagOptionResponse Prelude.Int
updateTagOptionResponse_httpStatus = Lens.lens (\UpdateTagOptionResponse' {httpStatus} -> httpStatus) (\s@UpdateTagOptionResponse' {} a -> s {httpStatus = a} :: UpdateTagOptionResponse)

instance Prelude.NFData UpdateTagOptionResponse
