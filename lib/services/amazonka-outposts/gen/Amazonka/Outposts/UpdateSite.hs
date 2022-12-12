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
-- Module      : Amazonka.Outposts.UpdateSite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified site.
module Amazonka.Outposts.UpdateSite
  ( -- * Creating a Request
    UpdateSite (..),
    newUpdateSite,

    -- * Request Lenses
    updateSite_description,
    updateSite_name,
    updateSite_notes,
    updateSite_siteId,

    -- * Destructuring the Response
    UpdateSiteResponse (..),
    newUpdateSiteResponse,

    -- * Response Lenses
    updateSiteResponse_site,
    updateSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSite' smart constructor.
data UpdateSite = UpdateSite'
  { description :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Maybe Prelude.Text,
    -- | Notes about a site.
    notes :: Prelude.Maybe Prelude.Text,
    -- | The ID or the Amazon Resource Name (ARN) of the site.
    siteId :: Prelude.Text
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
-- 'description', 'updateSite_description' - Undocumented member.
--
-- 'name', 'updateSite_name' - Undocumented member.
--
-- 'notes', 'updateSite_notes' - Notes about a site.
--
-- 'siteId', 'updateSite_siteId' - The ID or the Amazon Resource Name (ARN) of the site.
newUpdateSite ::
  -- | 'siteId'
  Prelude.Text ->
  UpdateSite
newUpdateSite pSiteId_ =
  UpdateSite'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      notes = Prelude.Nothing,
      siteId = pSiteId_
    }

-- | Undocumented member.
updateSite_description :: Lens.Lens' UpdateSite (Prelude.Maybe Prelude.Text)
updateSite_description = Lens.lens (\UpdateSite' {description} -> description) (\s@UpdateSite' {} a -> s {description = a} :: UpdateSite)

-- | Undocumented member.
updateSite_name :: Lens.Lens' UpdateSite (Prelude.Maybe Prelude.Text)
updateSite_name = Lens.lens (\UpdateSite' {name} -> name) (\s@UpdateSite' {} a -> s {name = a} :: UpdateSite)

-- | Notes about a site.
updateSite_notes :: Lens.Lens' UpdateSite (Prelude.Maybe Prelude.Text)
updateSite_notes = Lens.lens (\UpdateSite' {notes} -> notes) (\s@UpdateSite' {} a -> s {notes = a} :: UpdateSite)

-- | The ID or the Amazon Resource Name (ARN) of the site.
updateSite_siteId :: Lens.Lens' UpdateSite Prelude.Text
updateSite_siteId = Lens.lens (\UpdateSite' {siteId} -> siteId) (\s@UpdateSite' {} a -> s {siteId = a} :: UpdateSite)

instance Core.AWSRequest UpdateSite where
  type AWSResponse UpdateSite = UpdateSiteResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSiteResponse'
            Prelude.<$> (x Data..?> "Site")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSite where
  hashWithSalt _salt UpdateSite' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` siteId

instance Prelude.NFData UpdateSite where
  rnf UpdateSite' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf siteId

instance Data.ToHeaders UpdateSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSite where
  toJSON UpdateSite' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("Notes" Data..=) Prelude.<$> notes
          ]
      )

instance Data.ToPath UpdateSite where
  toPath UpdateSite' {..} =
    Prelude.mconcat ["/sites/", Data.toBS siteId]

instance Data.ToQuery UpdateSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSiteResponse' smart constructor.
data UpdateSiteResponse = UpdateSiteResponse'
  { site :: Prelude.Maybe Site,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'site', 'updateSiteResponse_site' - Undocumented member.
--
-- 'httpStatus', 'updateSiteResponse_httpStatus' - The response's http status code.
newUpdateSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSiteResponse
newUpdateSiteResponse pHttpStatus_ =
  UpdateSiteResponse'
    { site = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateSiteResponse_site :: Lens.Lens' UpdateSiteResponse (Prelude.Maybe Site)
updateSiteResponse_site = Lens.lens (\UpdateSiteResponse' {site} -> site) (\s@UpdateSiteResponse' {} a -> s {site = a} :: UpdateSiteResponse)

-- | The response's http status code.
updateSiteResponse_httpStatus :: Lens.Lens' UpdateSiteResponse Prelude.Int
updateSiteResponse_httpStatus = Lens.lens (\UpdateSiteResponse' {httpStatus} -> httpStatus) (\s@UpdateSiteResponse' {} a -> s {httpStatus = a} :: UpdateSiteResponse)

instance Prelude.NFData UpdateSiteResponse where
  rnf UpdateSiteResponse' {..} =
    Prelude.rnf site
      `Prelude.seq` Prelude.rnf httpStatus
