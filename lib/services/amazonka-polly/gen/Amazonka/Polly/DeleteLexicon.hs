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
-- Module      : Amazonka.Polly.DeleteLexicon
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified pronunciation lexicon stored in an Amazon Web
-- Services Region. A lexicon which has been deleted is not available for
-- speech synthesis, nor is it possible to retrieve it using either the
-- @GetLexicon@ or @ListLexicon@ APIs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons>.
module Amazonka.Polly.DeleteLexicon
  ( -- * Creating a Request
    DeleteLexicon (..),
    newDeleteLexicon,

    -- * Request Lenses
    deleteLexicon_name,

    -- * Destructuring the Response
    DeleteLexiconResponse (..),
    newDeleteLexiconResponse,

    -- * Response Lenses
    deleteLexiconResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Polly.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLexicon' smart constructor.
data DeleteLexicon = DeleteLexicon'
  { -- | The name of the lexicon to delete. Must be an existing lexicon in the
    -- region.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLexicon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteLexicon_name' - The name of the lexicon to delete. Must be an existing lexicon in the
-- region.
newDeleteLexicon ::
  -- | 'name'
  Prelude.Text ->
  DeleteLexicon
newDeleteLexicon pName_ =
  DeleteLexicon' {name = pName_}

-- | The name of the lexicon to delete. Must be an existing lexicon in the
-- region.
deleteLexicon_name :: Lens.Lens' DeleteLexicon Prelude.Text
deleteLexicon_name = Lens.lens (\DeleteLexicon' {name} -> name) (\s@DeleteLexicon' {} a -> s {name = a} :: DeleteLexicon)

instance Core.AWSRequest DeleteLexicon where
  type
    AWSResponse DeleteLexicon =
      DeleteLexiconResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLexiconResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLexicon where
  hashWithSalt _salt DeleteLexicon' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteLexicon where
  rnf DeleteLexicon' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteLexicon where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteLexicon where
  toPath DeleteLexicon' {..} =
    Prelude.mconcat ["/v1/lexicons/", Core.toBS name]

instance Core.ToQuery DeleteLexicon where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLexiconResponse' smart constructor.
data DeleteLexiconResponse = DeleteLexiconResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLexiconResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLexiconResponse_httpStatus' - The response's http status code.
newDeleteLexiconResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLexiconResponse
newDeleteLexiconResponse pHttpStatus_ =
  DeleteLexiconResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLexiconResponse_httpStatus :: Lens.Lens' DeleteLexiconResponse Prelude.Int
deleteLexiconResponse_httpStatus = Lens.lens (\DeleteLexiconResponse' {httpStatus} -> httpStatus) (\s@DeleteLexiconResponse' {} a -> s {httpStatus = a} :: DeleteLexiconResponse)

instance Prelude.NFData DeleteLexiconResponse where
  rnf DeleteLexiconResponse' {..} =
    Prelude.rnf httpStatus
