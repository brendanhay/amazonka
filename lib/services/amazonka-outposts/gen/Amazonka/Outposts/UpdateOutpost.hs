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
-- Module      : Amazonka.Outposts.UpdateOutpost
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Outpost.
module Amazonka.Outposts.UpdateOutpost
  ( -- * Creating a Request
    UpdateOutpost (..),
    newUpdateOutpost,

    -- * Request Lenses
    updateOutpost_name,
    updateOutpost_supportedHardwareType,
    updateOutpost_description,
    updateOutpost_outpostId,

    -- * Destructuring the Response
    UpdateOutpostResponse (..),
    newUpdateOutpostResponse,

    -- * Response Lenses
    updateOutpostResponse_outpost,
    updateOutpostResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateOutpost' smart constructor.
data UpdateOutpost = UpdateOutpost'
  { name :: Prelude.Maybe Prelude.Text,
    -- | The type of hardware for this Outpost.
    supportedHardwareType :: Prelude.Maybe SupportedHardwareType,
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID or the Amazon Resource Name (ARN) of the Outpost.
    outpostId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOutpost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateOutpost_name' - Undocumented member.
--
-- 'supportedHardwareType', 'updateOutpost_supportedHardwareType' - The type of hardware for this Outpost.
--
-- 'description', 'updateOutpost_description' - Undocumented member.
--
-- 'outpostId', 'updateOutpost_outpostId' - The ID or the Amazon Resource Name (ARN) of the Outpost.
newUpdateOutpost ::
  -- | 'outpostId'
  Prelude.Text ->
  UpdateOutpost
newUpdateOutpost pOutpostId_ =
  UpdateOutpost'
    { name = Prelude.Nothing,
      supportedHardwareType = Prelude.Nothing,
      description = Prelude.Nothing,
      outpostId = pOutpostId_
    }

-- | Undocumented member.
updateOutpost_name :: Lens.Lens' UpdateOutpost (Prelude.Maybe Prelude.Text)
updateOutpost_name = Lens.lens (\UpdateOutpost' {name} -> name) (\s@UpdateOutpost' {} a -> s {name = a} :: UpdateOutpost)

-- | The type of hardware for this Outpost.
updateOutpost_supportedHardwareType :: Lens.Lens' UpdateOutpost (Prelude.Maybe SupportedHardwareType)
updateOutpost_supportedHardwareType = Lens.lens (\UpdateOutpost' {supportedHardwareType} -> supportedHardwareType) (\s@UpdateOutpost' {} a -> s {supportedHardwareType = a} :: UpdateOutpost)

-- | Undocumented member.
updateOutpost_description :: Lens.Lens' UpdateOutpost (Prelude.Maybe Prelude.Text)
updateOutpost_description = Lens.lens (\UpdateOutpost' {description} -> description) (\s@UpdateOutpost' {} a -> s {description = a} :: UpdateOutpost)

-- | The ID or the Amazon Resource Name (ARN) of the Outpost.
updateOutpost_outpostId :: Lens.Lens' UpdateOutpost Prelude.Text
updateOutpost_outpostId = Lens.lens (\UpdateOutpost' {outpostId} -> outpostId) (\s@UpdateOutpost' {} a -> s {outpostId = a} :: UpdateOutpost)

instance Core.AWSRequest UpdateOutpost where
  type
    AWSResponse UpdateOutpost =
      UpdateOutpostResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateOutpostResponse'
            Prelude.<$> (x Core..?> "Outpost")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateOutpost where
  hashWithSalt _salt UpdateOutpost' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` supportedHardwareType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` outpostId

instance Prelude.NFData UpdateOutpost where
  rnf UpdateOutpost' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf supportedHardwareType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf outpostId

instance Core.ToHeaders UpdateOutpost where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateOutpost where
  toJSON UpdateOutpost' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("SupportedHardwareType" Core..=)
              Prelude.<$> supportedHardwareType,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateOutpost where
  toPath UpdateOutpost' {..} =
    Prelude.mconcat ["/outposts/", Core.toBS outpostId]

instance Core.ToQuery UpdateOutpost where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOutpostResponse' smart constructor.
data UpdateOutpostResponse = UpdateOutpostResponse'
  { outpost :: Prelude.Maybe Outpost,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOutpostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpost', 'updateOutpostResponse_outpost' - Undocumented member.
--
-- 'httpStatus', 'updateOutpostResponse_httpStatus' - The response's http status code.
newUpdateOutpostResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOutpostResponse
newUpdateOutpostResponse pHttpStatus_ =
  UpdateOutpostResponse'
    { outpost = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateOutpostResponse_outpost :: Lens.Lens' UpdateOutpostResponse (Prelude.Maybe Outpost)
updateOutpostResponse_outpost = Lens.lens (\UpdateOutpostResponse' {outpost} -> outpost) (\s@UpdateOutpostResponse' {} a -> s {outpost = a} :: UpdateOutpostResponse)

-- | The response's http status code.
updateOutpostResponse_httpStatus :: Lens.Lens' UpdateOutpostResponse Prelude.Int
updateOutpostResponse_httpStatus = Lens.lens (\UpdateOutpostResponse' {httpStatus} -> httpStatus) (\s@UpdateOutpostResponse' {} a -> s {httpStatus = a} :: UpdateOutpostResponse)

instance Prelude.NFData UpdateOutpostResponse where
  rnf UpdateOutpostResponse' {..} =
    Prelude.rnf outpost
      `Prelude.seq` Prelude.rnf httpStatus
