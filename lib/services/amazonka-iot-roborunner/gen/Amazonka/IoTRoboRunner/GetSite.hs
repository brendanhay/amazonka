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
-- Module      : Amazonka.IoTRoboRunner.GetSite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to get a site
module Amazonka.IoTRoboRunner.GetSite
  ( -- * Creating a Request
    GetSite (..),
    newGetSite,

    -- * Request Lenses
    getSite_id,

    -- * Destructuring the Response
    GetSiteResponse (..),
    newGetSiteResponse,

    -- * Response Lenses
    getSiteResponse_description,
    getSiteResponse_httpStatus,
    getSiteResponse_arn,
    getSiteResponse_id,
    getSiteResponse_name,
    getSiteResponse_countryCode,
    getSiteResponse_createdAt,
    getSiteResponse_updatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSite' smart constructor.
data GetSite = GetSite'
  { id :: Prelude.Text
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
-- 'id', 'getSite_id' - Undocumented member.
newGetSite ::
  -- | 'id'
  Prelude.Text ->
  GetSite
newGetSite pId_ = GetSite' {id = pId_}

-- | Undocumented member.
getSite_id :: Lens.Lens' GetSite Prelude.Text
getSite_id = Lens.lens (\GetSite' {id} -> id) (\s@GetSite' {} a -> s {id = a} :: GetSite)

instance Core.AWSRequest GetSite where
  type AWSResponse GetSite = GetSiteResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSiteResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "countryCode")
            Prelude.<*> (x Data..:> "createdAt")
            Prelude.<*> (x Data..:> "updatedAt")
      )

instance Prelude.Hashable GetSite where
  hashWithSalt _salt GetSite' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetSite where
  rnf GetSite' {..} = Prelude.rnf id

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
  toPath = Prelude.const "/getSite"

instance Data.ToQuery GetSite where
  toQuery GetSite' {..} =
    Prelude.mconcat ["id" Data.=: id]

-- | /See:/ 'newGetSiteResponse' smart constructor.
data GetSiteResponse = GetSiteResponse'
  { description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    name :: Prelude.Text,
    countryCode :: Prelude.Text,
    createdAt :: Data.POSIX,
    updatedAt :: Data.POSIX
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
-- 'description', 'getSiteResponse_description' - Undocumented member.
--
-- 'httpStatus', 'getSiteResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getSiteResponse_arn' - Undocumented member.
--
-- 'id', 'getSiteResponse_id' - Undocumented member.
--
-- 'name', 'getSiteResponse_name' - Undocumented member.
--
-- 'countryCode', 'getSiteResponse_countryCode' - Undocumented member.
--
-- 'createdAt', 'getSiteResponse_createdAt' - Undocumented member.
--
-- 'updatedAt', 'getSiteResponse_updatedAt' - Undocumented member.
newGetSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'countryCode'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  GetSiteResponse
newGetSiteResponse
  pHttpStatus_
  pArn_
  pId_
  pName_
  pCountryCode_
  pCreatedAt_
  pUpdatedAt_ =
    GetSiteResponse'
      { description = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        name = pName_,
        countryCode = pCountryCode_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | Undocumented member.
getSiteResponse_description :: Lens.Lens' GetSiteResponse (Prelude.Maybe Prelude.Text)
getSiteResponse_description = Lens.lens (\GetSiteResponse' {description} -> description) (\s@GetSiteResponse' {} a -> s {description = a} :: GetSiteResponse)

-- | The response's http status code.
getSiteResponse_httpStatus :: Lens.Lens' GetSiteResponse Prelude.Int
getSiteResponse_httpStatus = Lens.lens (\GetSiteResponse' {httpStatus} -> httpStatus) (\s@GetSiteResponse' {} a -> s {httpStatus = a} :: GetSiteResponse)

-- | Undocumented member.
getSiteResponse_arn :: Lens.Lens' GetSiteResponse Prelude.Text
getSiteResponse_arn = Lens.lens (\GetSiteResponse' {arn} -> arn) (\s@GetSiteResponse' {} a -> s {arn = a} :: GetSiteResponse)

-- | Undocumented member.
getSiteResponse_id :: Lens.Lens' GetSiteResponse Prelude.Text
getSiteResponse_id = Lens.lens (\GetSiteResponse' {id} -> id) (\s@GetSiteResponse' {} a -> s {id = a} :: GetSiteResponse)

-- | Undocumented member.
getSiteResponse_name :: Lens.Lens' GetSiteResponse Prelude.Text
getSiteResponse_name = Lens.lens (\GetSiteResponse' {name} -> name) (\s@GetSiteResponse' {} a -> s {name = a} :: GetSiteResponse)

-- | Undocumented member.
getSiteResponse_countryCode :: Lens.Lens' GetSiteResponse Prelude.Text
getSiteResponse_countryCode = Lens.lens (\GetSiteResponse' {countryCode} -> countryCode) (\s@GetSiteResponse' {} a -> s {countryCode = a} :: GetSiteResponse)

-- | Undocumented member.
getSiteResponse_createdAt :: Lens.Lens' GetSiteResponse Prelude.UTCTime
getSiteResponse_createdAt = Lens.lens (\GetSiteResponse' {createdAt} -> createdAt) (\s@GetSiteResponse' {} a -> s {createdAt = a} :: GetSiteResponse) Prelude.. Data._Time

-- | Undocumented member.
getSiteResponse_updatedAt :: Lens.Lens' GetSiteResponse Prelude.UTCTime
getSiteResponse_updatedAt = Lens.lens (\GetSiteResponse' {updatedAt} -> updatedAt) (\s@GetSiteResponse' {} a -> s {updatedAt = a} :: GetSiteResponse) Prelude.. Data._Time

instance Prelude.NFData GetSiteResponse where
  rnf GetSiteResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf arn `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf countryCode `Prelude.seq`
                Prelude.rnf createdAt `Prelude.seq`
                  Prelude.rnf updatedAt
