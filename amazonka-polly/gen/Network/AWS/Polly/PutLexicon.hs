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
-- Module      : Network.AWS.Polly.PutLexicon
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a pronunciation lexicon in an AWS Region. If a lexicon with the
-- same name already exists in the region, it is overwritten by the new
-- lexicon. Lexicon operations have eventual consistency, therefore, it
-- might take some time before the lexicon is available to the
-- SynthesizeSpeech operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons>.
module Network.AWS.Polly.PutLexicon
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutLexicon' smart constructor.
data PutLexicon = PutLexicon'
  { -- | Name of the lexicon. The name must follow the regular express format
    -- [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric
    -- string up to 20 characters long.
    name :: Core.Text,
    -- | Content of the PLS lexicon as string data.
    content :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'content'
  Core.Text ->
  PutLexicon
newPutLexicon pName_ pContent_ =
  PutLexicon'
    { name = pName_,
      content = Core._Sensitive Lens.# pContent_
    }

-- | Name of the lexicon. The name must follow the regular express format
-- [0-9A-Za-z]{1,20}. That is, the name is a case-sensitive alphanumeric
-- string up to 20 characters long.
putLexicon_name :: Lens.Lens' PutLexicon Core.Text
putLexicon_name = Lens.lens (\PutLexicon' {name} -> name) (\s@PutLexicon' {} a -> s {name = a} :: PutLexicon)

-- | Content of the PLS lexicon as string data.
putLexicon_content :: Lens.Lens' PutLexicon Core.Text
putLexicon_content = Lens.lens (\PutLexicon' {content} -> content) (\s@PutLexicon' {} a -> s {content = a} :: PutLexicon) Core.. Core._Sensitive

instance Core.AWSRequest PutLexicon where
  type AWSResponse PutLexicon = PutLexiconResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutLexiconResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutLexicon

instance Core.NFData PutLexicon

instance Core.ToHeaders PutLexicon where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON PutLexicon where
  toJSON PutLexicon' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Content" Core..= content)]
      )

instance Core.ToPath PutLexicon where
  toPath PutLexicon' {..} =
    Core.mconcat ["/v1/lexicons/", Core.toBS name]

instance Core.ToQuery PutLexicon where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutLexiconResponse' smart constructor.
data PutLexiconResponse = PutLexiconResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PutLexiconResponse
newPutLexiconResponse pHttpStatus_ =
  PutLexiconResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putLexiconResponse_httpStatus :: Lens.Lens' PutLexiconResponse Core.Int
putLexiconResponse_httpStatus = Lens.lens (\PutLexiconResponse' {httpStatus} -> httpStatus) (\s@PutLexiconResponse' {} a -> s {httpStatus = a} :: PutLexiconResponse)

instance Core.NFData PutLexiconResponse
