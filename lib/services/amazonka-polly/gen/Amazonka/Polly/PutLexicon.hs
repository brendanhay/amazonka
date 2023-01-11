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
-- Module      : Amazonka.Polly.PutLexicon
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a pronunciation lexicon in an Amazon Web Services Region. If a
-- lexicon with the same name already exists in the region, it is
-- overwritten by the new lexicon. Lexicon operations have eventual
-- consistency, therefore, it might take some time before the lexicon is
-- available to the SynthesizeSpeech operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons>.
module Amazonka.Polly.PutLexicon
  ( -- * Creating a Request
    PutLexicon (..),
    newPutLexicon,

    -- * Request Lenses
    putLexicon_name,
    putLexicon_content,

    -- * Destructuring the Response
    PutLexiconResponse (..),
    newPutLexiconResponse,

    -- * Response Lenses
    putLexiconResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Polly.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLexicon' smart constructor.
data PutLexicon = PutLexicon'
  { -- | Name of the lexicon. The name must follow the regular express format
    -- [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric
    -- string up to 20 characters long.
    name :: Prelude.Text,
    -- | Content of the PLS lexicon as string data.
    content :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLexicon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'putLexicon_name' - Name of the lexicon. The name must follow the regular express format
-- [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric
-- string up to 20 characters long.
--
-- 'content', 'putLexicon_content' - Content of the PLS lexicon as string data.
newPutLexicon ::
  -- | 'name'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  PutLexicon
newPutLexicon pName_ pContent_ =
  PutLexicon'
    { name = pName_,
      content = Data._Sensitive Lens.# pContent_
    }

-- | Name of the lexicon. The name must follow the regular express format
-- [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric
-- string up to 20 characters long.
putLexicon_name :: Lens.Lens' PutLexicon Prelude.Text
putLexicon_name = Lens.lens (\PutLexicon' {name} -> name) (\s@PutLexicon' {} a -> s {name = a} :: PutLexicon)

-- | Content of the PLS lexicon as string data.
putLexicon_content :: Lens.Lens' PutLexicon Prelude.Text
putLexicon_content = Lens.lens (\PutLexicon' {content} -> content) (\s@PutLexicon' {} a -> s {content = a} :: PutLexicon) Prelude.. Data._Sensitive

instance Core.AWSRequest PutLexicon where
  type AWSResponse PutLexicon = PutLexiconResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutLexiconResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLexicon where
  hashWithSalt _salt PutLexicon' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` content

instance Prelude.NFData PutLexicon where
  rnf PutLexicon' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf content

instance Data.ToHeaders PutLexicon where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutLexicon where
  toJSON PutLexicon' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Content" Data..= content)]
      )

instance Data.ToPath PutLexicon where
  toPath PutLexicon' {..} =
    Prelude.mconcat ["/v1/lexicons/", Data.toBS name]

instance Data.ToQuery PutLexicon where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLexiconResponse' smart constructor.
data PutLexiconResponse = PutLexiconResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLexiconResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putLexiconResponse_httpStatus' - The response's http status code.
newPutLexiconResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutLexiconResponse
newPutLexiconResponse pHttpStatus_ =
  PutLexiconResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putLexiconResponse_httpStatus :: Lens.Lens' PutLexiconResponse Prelude.Int
putLexiconResponse_httpStatus = Lens.lens (\PutLexiconResponse' {httpStatus} -> httpStatus) (\s@PutLexiconResponse' {} a -> s {httpStatus = a} :: PutLexiconResponse)

instance Prelude.NFData PutLexiconResponse where
  rnf PutLexiconResponse' {..} = Prelude.rnf httpStatus
