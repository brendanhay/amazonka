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
-- Module      : Amazonka.IoTRoboRunner.UpdateSite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to update a site
module Amazonka.IoTRoboRunner.UpdateSite
  ( -- * Creating a Request
    UpdateSite (..),
    newUpdateSite,

    -- * Request Lenses
    updateSite_name,
    updateSite_countryCode,
    updateSite_description,
    updateSite_id,

    -- * Destructuring the Response
    UpdateSiteResponse (..),
    newUpdateSiteResponse,

    -- * Response Lenses
    updateSiteResponse_countryCode,
    updateSiteResponse_description,
    updateSiteResponse_httpStatus,
    updateSiteResponse_arn,
    updateSiteResponse_id,
    updateSiteResponse_name,
    updateSiteResponse_updatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSite' smart constructor.
data UpdateSite = UpdateSite'
  { name :: Prelude.Maybe Prelude.Text,
    countryCode :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateSite_name' - Undocumented member.
--
-- 'countryCode', 'updateSite_countryCode' - Undocumented member.
--
-- 'description', 'updateSite_description' - Undocumented member.
--
-- 'id', 'updateSite_id' - Undocumented member.
newUpdateSite ::
  -- | 'id'
  Prelude.Text ->
  UpdateSite
newUpdateSite pId_ =
  UpdateSite'
    { name = Prelude.Nothing,
      countryCode = Prelude.Nothing,
      description = Prelude.Nothing,
      id = pId_
    }

-- | Undocumented member.
updateSite_name :: Lens.Lens' UpdateSite (Prelude.Maybe Prelude.Text)
updateSite_name = Lens.lens (\UpdateSite' {name} -> name) (\s@UpdateSite' {} a -> s {name = a} :: UpdateSite)

-- | Undocumented member.
updateSite_countryCode :: Lens.Lens' UpdateSite (Prelude.Maybe Prelude.Text)
updateSite_countryCode = Lens.lens (\UpdateSite' {countryCode} -> countryCode) (\s@UpdateSite' {} a -> s {countryCode = a} :: UpdateSite)

-- | Undocumented member.
updateSite_description :: Lens.Lens' UpdateSite (Prelude.Maybe Prelude.Text)
updateSite_description = Lens.lens (\UpdateSite' {description} -> description) (\s@UpdateSite' {} a -> s {description = a} :: UpdateSite)

-- | Undocumented member.
updateSite_id :: Lens.Lens' UpdateSite Prelude.Text
updateSite_id = Lens.lens (\UpdateSite' {id} -> id) (\s@UpdateSite' {} a -> s {id = a} :: UpdateSite)

instance Core.AWSRequest UpdateSite where
  type AWSResponse UpdateSite = UpdateSiteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSiteResponse'
            Prelude.<$> (x Core..?> "countryCode")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "id")
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> (x Core..:> "updatedAt")
      )

instance Prelude.Hashable UpdateSite where
  hashWithSalt _salt UpdateSite' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateSite where
  rnf UpdateSite' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders UpdateSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSite where
  toJSON UpdateSite' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("countryCode" Core..=) Prelude.<$> countryCode,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath UpdateSite where
  toPath = Prelude.const "/updateSite"

instance Core.ToQuery UpdateSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSiteResponse' smart constructor.
data UpdateSiteResponse = UpdateSiteResponse'
  { countryCode :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    name :: Prelude.Text,
    updatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countryCode', 'updateSiteResponse_countryCode' - Undocumented member.
--
-- 'description', 'updateSiteResponse_description' - Undocumented member.
--
-- 'httpStatus', 'updateSiteResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'updateSiteResponse_arn' - Undocumented member.
--
-- 'id', 'updateSiteResponse_id' - Undocumented member.
--
-- 'name', 'updateSiteResponse_name' - Undocumented member.
--
-- 'updatedAt', 'updateSiteResponse_updatedAt' - Undocumented member.
newUpdateSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  UpdateSiteResponse
newUpdateSiteResponse
  pHttpStatus_
  pArn_
  pId_
  pName_
  pUpdatedAt_ =
    UpdateSiteResponse'
      { countryCode = Prelude.Nothing,
        description = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        name = pName_,
        updatedAt = Core._Time Lens.# pUpdatedAt_
      }

-- | Undocumented member.
updateSiteResponse_countryCode :: Lens.Lens' UpdateSiteResponse (Prelude.Maybe Prelude.Text)
updateSiteResponse_countryCode = Lens.lens (\UpdateSiteResponse' {countryCode} -> countryCode) (\s@UpdateSiteResponse' {} a -> s {countryCode = a} :: UpdateSiteResponse)

-- | Undocumented member.
updateSiteResponse_description :: Lens.Lens' UpdateSiteResponse (Prelude.Maybe Prelude.Text)
updateSiteResponse_description = Lens.lens (\UpdateSiteResponse' {description} -> description) (\s@UpdateSiteResponse' {} a -> s {description = a} :: UpdateSiteResponse)

-- | The response's http status code.
updateSiteResponse_httpStatus :: Lens.Lens' UpdateSiteResponse Prelude.Int
updateSiteResponse_httpStatus = Lens.lens (\UpdateSiteResponse' {httpStatus} -> httpStatus) (\s@UpdateSiteResponse' {} a -> s {httpStatus = a} :: UpdateSiteResponse)

-- | Undocumented member.
updateSiteResponse_arn :: Lens.Lens' UpdateSiteResponse Prelude.Text
updateSiteResponse_arn = Lens.lens (\UpdateSiteResponse' {arn} -> arn) (\s@UpdateSiteResponse' {} a -> s {arn = a} :: UpdateSiteResponse)

-- | Undocumented member.
updateSiteResponse_id :: Lens.Lens' UpdateSiteResponse Prelude.Text
updateSiteResponse_id = Lens.lens (\UpdateSiteResponse' {id} -> id) (\s@UpdateSiteResponse' {} a -> s {id = a} :: UpdateSiteResponse)

-- | Undocumented member.
updateSiteResponse_name :: Lens.Lens' UpdateSiteResponse Prelude.Text
updateSiteResponse_name = Lens.lens (\UpdateSiteResponse' {name} -> name) (\s@UpdateSiteResponse' {} a -> s {name = a} :: UpdateSiteResponse)

-- | Undocumented member.
updateSiteResponse_updatedAt :: Lens.Lens' UpdateSiteResponse Prelude.UTCTime
updateSiteResponse_updatedAt = Lens.lens (\UpdateSiteResponse' {updatedAt} -> updatedAt) (\s@UpdateSiteResponse' {} a -> s {updatedAt = a} :: UpdateSiteResponse) Prelude.. Core._Time

instance Prelude.NFData UpdateSiteResponse where
  rnf UpdateSiteResponse' {..} =
    Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedAt
