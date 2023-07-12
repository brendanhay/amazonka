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
-- Module      : Amazonka.IoTRoboRunner.CreateSite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to create a site
module Amazonka.IoTRoboRunner.CreateSite
  ( -- * Creating a Request
    CreateSite (..),
    newCreateSite,

    -- * Request Lenses
    createSite_clientToken,
    createSite_description,
    createSite_name,
    createSite_countryCode,

    -- * Destructuring the Response
    CreateSiteResponse (..),
    newCreateSiteResponse,

    -- * Response Lenses
    createSiteResponse_httpStatus,
    createSiteResponse_arn,
    createSiteResponse_id,
    createSiteResponse_createdAt,
    createSiteResponse_updatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSite' smart constructor.
data CreateSite = CreateSite'
  { clientToken :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Text,
    countryCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createSite_clientToken' - Undocumented member.
--
-- 'description', 'createSite_description' - Undocumented member.
--
-- 'name', 'createSite_name' - Undocumented member.
--
-- 'countryCode', 'createSite_countryCode' - Undocumented member.
newCreateSite ::
  -- | 'name'
  Prelude.Text ->
  -- | 'countryCode'
  Prelude.Text ->
  CreateSite
newCreateSite pName_ pCountryCode_ =
  CreateSite'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      countryCode = pCountryCode_
    }

-- | Undocumented member.
createSite_clientToken :: Lens.Lens' CreateSite (Prelude.Maybe Prelude.Text)
createSite_clientToken = Lens.lens (\CreateSite' {clientToken} -> clientToken) (\s@CreateSite' {} a -> s {clientToken = a} :: CreateSite)

-- | Undocumented member.
createSite_description :: Lens.Lens' CreateSite (Prelude.Maybe Prelude.Text)
createSite_description = Lens.lens (\CreateSite' {description} -> description) (\s@CreateSite' {} a -> s {description = a} :: CreateSite)

-- | Undocumented member.
createSite_name :: Lens.Lens' CreateSite Prelude.Text
createSite_name = Lens.lens (\CreateSite' {name} -> name) (\s@CreateSite' {} a -> s {name = a} :: CreateSite)

-- | Undocumented member.
createSite_countryCode :: Lens.Lens' CreateSite Prelude.Text
createSite_countryCode = Lens.lens (\CreateSite' {countryCode} -> countryCode) (\s@CreateSite' {} a -> s {countryCode = a} :: CreateSite)

instance Core.AWSRequest CreateSite where
  type AWSResponse CreateSite = CreateSiteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSiteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "createdAt")
            Prelude.<*> (x Data..:> "updatedAt")
      )

instance Prelude.Hashable CreateSite where
  hashWithSalt _salt CreateSite' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` countryCode

instance Prelude.NFData CreateSite where
  rnf CreateSite' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf countryCode

instance Data.ToHeaders CreateSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSite where
  toJSON CreateSite' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("countryCode" Data..= countryCode)
          ]
      )

instance Data.ToPath CreateSite where
  toPath = Prelude.const "/createSite"

instance Data.ToQuery CreateSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSiteResponse' smart constructor.
data CreateSiteResponse = CreateSiteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    createdAt :: Data.POSIX,
    updatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSiteResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createSiteResponse_arn' - Undocumented member.
--
-- 'id', 'createSiteResponse_id' - Undocumented member.
--
-- 'createdAt', 'createSiteResponse_createdAt' - Undocumented member.
--
-- 'updatedAt', 'createSiteResponse_updatedAt' - Undocumented member.
newCreateSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  CreateSiteResponse
newCreateSiteResponse
  pHttpStatus_
  pArn_
  pId_
  pCreatedAt_
  pUpdatedAt_ =
    CreateSiteResponse'
      { httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | The response's http status code.
createSiteResponse_httpStatus :: Lens.Lens' CreateSiteResponse Prelude.Int
createSiteResponse_httpStatus = Lens.lens (\CreateSiteResponse' {httpStatus} -> httpStatus) (\s@CreateSiteResponse' {} a -> s {httpStatus = a} :: CreateSiteResponse)

-- | Undocumented member.
createSiteResponse_arn :: Lens.Lens' CreateSiteResponse Prelude.Text
createSiteResponse_arn = Lens.lens (\CreateSiteResponse' {arn} -> arn) (\s@CreateSiteResponse' {} a -> s {arn = a} :: CreateSiteResponse)

-- | Undocumented member.
createSiteResponse_id :: Lens.Lens' CreateSiteResponse Prelude.Text
createSiteResponse_id = Lens.lens (\CreateSiteResponse' {id} -> id) (\s@CreateSiteResponse' {} a -> s {id = a} :: CreateSiteResponse)

-- | Undocumented member.
createSiteResponse_createdAt :: Lens.Lens' CreateSiteResponse Prelude.UTCTime
createSiteResponse_createdAt = Lens.lens (\CreateSiteResponse' {createdAt} -> createdAt) (\s@CreateSiteResponse' {} a -> s {createdAt = a} :: CreateSiteResponse) Prelude.. Data._Time

-- | Undocumented member.
createSiteResponse_updatedAt :: Lens.Lens' CreateSiteResponse Prelude.UTCTime
createSiteResponse_updatedAt = Lens.lens (\CreateSiteResponse' {updatedAt} -> updatedAt) (\s@CreateSiteResponse' {} a -> s {updatedAt = a} :: CreateSiteResponse) Prelude.. Data._Time

instance Prelude.NFData CreateSiteResponse where
  rnf CreateSiteResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
