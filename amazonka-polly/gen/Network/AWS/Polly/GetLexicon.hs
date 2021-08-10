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
-- Module      : Network.AWS.Polly.GetLexicon
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the content of the specified pronunciation lexicon stored in an
-- AWS Region. For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons>.
module Network.AWS.Polly.GetLexicon
  ( -- * Creating a Request
    GetLexicon (..),
    newGetLexicon,

    -- * Request Lenses
    getLexicon_name,

    -- * Destructuring the Response
    GetLexiconResponse (..),
    newGetLexiconResponse,

    -- * Response Lenses
    getLexiconResponse_lexiconAttributes,
    getLexiconResponse_lexicon,
    getLexiconResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLexicon' smart constructor.
data GetLexicon = GetLexicon'
  { -- | Name of the lexicon.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLexicon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getLexicon_name' - Name of the lexicon.
newGetLexicon ::
  -- | 'name'
  Prelude.Text ->
  GetLexicon
newGetLexicon pName_ = GetLexicon' {name = pName_}

-- | Name of the lexicon.
getLexicon_name :: Lens.Lens' GetLexicon Prelude.Text
getLexicon_name = Lens.lens (\GetLexicon' {name} -> name) (\s@GetLexicon' {} a -> s {name = a} :: GetLexicon)

instance Core.AWSRequest GetLexicon where
  type AWSResponse GetLexicon = GetLexiconResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLexiconResponse'
            Prelude.<$> (x Core..?> "LexiconAttributes")
            Prelude.<*> (x Core..?> "Lexicon")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLexicon

instance Prelude.NFData GetLexicon

instance Core.ToHeaders GetLexicon where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetLexicon where
  toPath GetLexicon' {..} =
    Prelude.mconcat ["/v1/lexicons/", Core.toBS name]

instance Core.ToQuery GetLexicon where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLexiconResponse' smart constructor.
data GetLexiconResponse = GetLexiconResponse'
  { -- | Metadata of the lexicon, including phonetic alphabetic used, language
    -- code, lexicon ARN, number of lexemes defined in the lexicon, and size of
    -- lexicon in bytes.
    lexiconAttributes :: Prelude.Maybe LexiconAttributes,
    -- | Lexicon object that provides name and the string content of the lexicon.
    lexicon :: Prelude.Maybe Lexicon,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLexiconResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lexiconAttributes', 'getLexiconResponse_lexiconAttributes' - Metadata of the lexicon, including phonetic alphabetic used, language
-- code, lexicon ARN, number of lexemes defined in the lexicon, and size of
-- lexicon in bytes.
--
-- 'lexicon', 'getLexiconResponse_lexicon' - Lexicon object that provides name and the string content of the lexicon.
--
-- 'httpStatus', 'getLexiconResponse_httpStatus' - The response's http status code.
newGetLexiconResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLexiconResponse
newGetLexiconResponse pHttpStatus_ =
  GetLexiconResponse'
    { lexiconAttributes =
        Prelude.Nothing,
      lexicon = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata of the lexicon, including phonetic alphabetic used, language
-- code, lexicon ARN, number of lexemes defined in the lexicon, and size of
-- lexicon in bytes.
getLexiconResponse_lexiconAttributes :: Lens.Lens' GetLexiconResponse (Prelude.Maybe LexiconAttributes)
getLexiconResponse_lexiconAttributes = Lens.lens (\GetLexiconResponse' {lexiconAttributes} -> lexiconAttributes) (\s@GetLexiconResponse' {} a -> s {lexiconAttributes = a} :: GetLexiconResponse)

-- | Lexicon object that provides name and the string content of the lexicon.
getLexiconResponse_lexicon :: Lens.Lens' GetLexiconResponse (Prelude.Maybe Lexicon)
getLexiconResponse_lexicon = Lens.lens (\GetLexiconResponse' {lexicon} -> lexicon) (\s@GetLexiconResponse' {} a -> s {lexicon = a} :: GetLexiconResponse)

-- | The response's http status code.
getLexiconResponse_httpStatus :: Lens.Lens' GetLexiconResponse Prelude.Int
getLexiconResponse_httpStatus = Lens.lens (\GetLexiconResponse' {httpStatus} -> httpStatus) (\s@GetLexiconResponse' {} a -> s {httpStatus = a} :: GetLexiconResponse)

instance Prelude.NFData GetLexiconResponse
