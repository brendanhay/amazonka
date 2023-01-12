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
-- Module      : Amazonka.Glue.DeleteCrawler
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified crawler from the Glue Data Catalog, unless the
-- crawler state is @RUNNING@.
module Amazonka.Glue.DeleteCrawler
  ( -- * Creating a Request
    DeleteCrawler (..),
    newDeleteCrawler,

    -- * Request Lenses
    deleteCrawler_name,

    -- * Destructuring the Response
    DeleteCrawlerResponse (..),
    newDeleteCrawlerResponse,

    -- * Response Lenses
    deleteCrawlerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCrawler' smart constructor.
data DeleteCrawler = DeleteCrawler'
  { -- | The name of the crawler to remove.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCrawler' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteCrawler_name' - The name of the crawler to remove.
newDeleteCrawler ::
  -- | 'name'
  Prelude.Text ->
  DeleteCrawler
newDeleteCrawler pName_ =
  DeleteCrawler' {name = pName_}

-- | The name of the crawler to remove.
deleteCrawler_name :: Lens.Lens' DeleteCrawler Prelude.Text
deleteCrawler_name = Lens.lens (\DeleteCrawler' {name} -> name) (\s@DeleteCrawler' {} a -> s {name = a} :: DeleteCrawler)

instance Core.AWSRequest DeleteCrawler where
  type
    AWSResponse DeleteCrawler =
      DeleteCrawlerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCrawlerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCrawler where
  hashWithSalt _salt DeleteCrawler' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteCrawler where
  rnf DeleteCrawler' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteCrawler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.DeleteCrawler" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCrawler where
  toJSON DeleteCrawler' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteCrawler where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCrawler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCrawlerResponse' smart constructor.
data DeleteCrawlerResponse = DeleteCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCrawlerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCrawlerResponse_httpStatus' - The response's http status code.
newDeleteCrawlerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCrawlerResponse
newDeleteCrawlerResponse pHttpStatus_ =
  DeleteCrawlerResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteCrawlerResponse_httpStatus :: Lens.Lens' DeleteCrawlerResponse Prelude.Int
deleteCrawlerResponse_httpStatus = Lens.lens (\DeleteCrawlerResponse' {httpStatus} -> httpStatus) (\s@DeleteCrawlerResponse' {} a -> s {httpStatus = a} :: DeleteCrawlerResponse)

instance Prelude.NFData DeleteCrawlerResponse where
  rnf DeleteCrawlerResponse' {..} =
    Prelude.rnf httpStatus
