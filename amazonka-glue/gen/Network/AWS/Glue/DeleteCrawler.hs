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
-- Module      : Network.AWS.Glue.DeleteCrawler
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified crawler from the AWS Glue Data Catalog, unless the
-- crawler state is @RUNNING@.
module Network.AWS.Glue.DeleteCrawler
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCrawler' smart constructor.
data DeleteCrawler = DeleteCrawler'
  { -- | The name of the crawler to remove.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteCrawler where
  type Rs DeleteCrawler = DeleteCrawlerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCrawlerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCrawler

instance Prelude.NFData DeleteCrawler

instance Prelude.ToHeaders DeleteCrawler where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.DeleteCrawler" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteCrawler where
  toJSON DeleteCrawler' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteCrawler where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteCrawler where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCrawlerResponse' smart constructor.
data DeleteCrawlerResponse = DeleteCrawlerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteCrawlerResponse
