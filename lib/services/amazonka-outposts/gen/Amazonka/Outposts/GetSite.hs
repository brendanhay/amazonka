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
-- Module      : Amazonka.Outposts.GetSite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified Outpost site.
module Amazonka.Outposts.GetSite
  ( -- * Creating a Request
    GetSite (..),
    newGetSite,

    -- * Request Lenses
    getSite_siteId,

    -- * Destructuring the Response
    GetSiteResponse (..),
    newGetSiteResponse,

    -- * Response Lenses
    getSiteResponse_site,
    getSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSite' smart constructor.
data GetSite = GetSite'
  { -- | The ID or the Amazon Resource Name (ARN) of the site.
    siteId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'siteId', 'getSite_siteId' - The ID or the Amazon Resource Name (ARN) of the site.
newGetSite ::
  -- | 'siteId'
  Prelude.Text ->
  GetSite
newGetSite pSiteId_ = GetSite' {siteId = pSiteId_}

-- | The ID or the Amazon Resource Name (ARN) of the site.
getSite_siteId :: Lens.Lens' GetSite Prelude.Text
getSite_siteId = Lens.lens (\GetSite' {siteId} -> siteId) (\s@GetSite' {} a -> s {siteId = a} :: GetSite)

instance Core.AWSRequest GetSite where
  type AWSResponse GetSite = GetSiteResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSiteResponse'
            Prelude.<$> (x Data..?> "Site")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSite where
  hashWithSalt _salt GetSite' {..} =
    _salt `Prelude.hashWithSalt` siteId

instance Prelude.NFData GetSite where
  rnf GetSite' {..} = Prelude.rnf siteId

instance Data.ToHeaders GetSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSite where
  toPath GetSite' {..} =
    Prelude.mconcat ["/sites/", Data.toBS siteId]

instance Data.ToQuery GetSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSiteResponse' smart constructor.
data GetSiteResponse = GetSiteResponse'
  { site :: Prelude.Maybe Site,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'site', 'getSiteResponse_site' - Undocumented member.
--
-- 'httpStatus', 'getSiteResponse_httpStatus' - The response's http status code.
newGetSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSiteResponse
newGetSiteResponse pHttpStatus_ =
  GetSiteResponse'
    { site = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getSiteResponse_site :: Lens.Lens' GetSiteResponse (Prelude.Maybe Site)
getSiteResponse_site = Lens.lens (\GetSiteResponse' {site} -> site) (\s@GetSiteResponse' {} a -> s {site = a} :: GetSiteResponse)

-- | The response's http status code.
getSiteResponse_httpStatus :: Lens.Lens' GetSiteResponse Prelude.Int
getSiteResponse_httpStatus = Lens.lens (\GetSiteResponse' {httpStatus} -> httpStatus) (\s@GetSiteResponse' {} a -> s {httpStatus = a} :: GetSiteResponse)

instance Prelude.NFData GetSiteResponse where
  rnf GetSiteResponse' {..} =
    Prelude.rnf site `Prelude.seq`
      Prelude.rnf httpStatus
