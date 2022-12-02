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
-- Module      : Amazonka.ServiceCatalog.DeletePortfolio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified portfolio.
--
-- You cannot delete a portfolio if it was shared with you or if it has
-- associated products, users, constraints, or shared accounts.
--
-- A delegated admin is authorized to invoke this command.
module Amazonka.ServiceCatalog.DeletePortfolio
  ( -- * Creating a Request
    DeletePortfolio (..),
    newDeletePortfolio,

    -- * Request Lenses
    deletePortfolio_acceptLanguage,
    deletePortfolio_id,

    -- * Destructuring the Response
    DeletePortfolioResponse (..),
    newDeletePortfolioResponse,

    -- * Response Lenses
    deletePortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDeletePortfolio' smart constructor.
data DeletePortfolio = DeletePortfolio'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'deletePortfolio_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'deletePortfolio_id' - The portfolio identifier.
newDeletePortfolio ::
  -- | 'id'
  Prelude.Text ->
  DeletePortfolio
newDeletePortfolio pId_ =
  DeletePortfolio'
    { acceptLanguage = Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deletePortfolio_acceptLanguage :: Lens.Lens' DeletePortfolio (Prelude.Maybe Prelude.Text)
deletePortfolio_acceptLanguage = Lens.lens (\DeletePortfolio' {acceptLanguage} -> acceptLanguage) (\s@DeletePortfolio' {} a -> s {acceptLanguage = a} :: DeletePortfolio)

-- | The portfolio identifier.
deletePortfolio_id :: Lens.Lens' DeletePortfolio Prelude.Text
deletePortfolio_id = Lens.lens (\DeletePortfolio' {id} -> id) (\s@DeletePortfolio' {} a -> s {id = a} :: DeletePortfolio)

instance Core.AWSRequest DeletePortfolio where
  type
    AWSResponse DeletePortfolio =
      DeletePortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePortfolio where
  hashWithSalt _salt DeletePortfolio' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeletePortfolio where
  rnf DeletePortfolio' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeletePortfolio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DeletePortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePortfolio where
  toJSON DeletePortfolio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DeletePortfolio where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePortfolioResponse' smart constructor.
data DeletePortfolioResponse = DeletePortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePortfolioResponse_httpStatus' - The response's http status code.
newDeletePortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePortfolioResponse
newDeletePortfolioResponse pHttpStatus_ =
  DeletePortfolioResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePortfolioResponse_httpStatus :: Lens.Lens' DeletePortfolioResponse Prelude.Int
deletePortfolioResponse_httpStatus = Lens.lens (\DeletePortfolioResponse' {httpStatus} -> httpStatus) (\s@DeletePortfolioResponse' {} a -> s {httpStatus = a} :: DeletePortfolioResponse)

instance Prelude.NFData DeletePortfolioResponse where
  rnf DeletePortfolioResponse' {..} =
    Prelude.rnf httpStatus
