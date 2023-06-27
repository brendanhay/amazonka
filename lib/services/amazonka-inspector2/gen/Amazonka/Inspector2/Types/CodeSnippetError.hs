{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Inspector2.Types.CodeSnippetError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CodeSnippetError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.CodeSnippetErrorCode
import qualified Amazonka.Prelude as Prelude

-- | Contains information about any errors encountered while trying to
-- retrieve a code snippet.
--
-- /See:/ 'newCodeSnippetError' smart constructor.
data CodeSnippetError = CodeSnippetError'
  { -- | The error code for the error that prevented a code snippet from being
    -- retrieved.
    errorCode :: CodeSnippetErrorCode,
    -- | The error message received when Amazon Inspector failed to retrieve a
    -- code snippet.
    errorMessage :: Prelude.Text,
    -- | The ARN of the finding that a code snippet couldn\'t be retrieved for.
    findingArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeSnippetError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'codeSnippetError_errorCode' - The error code for the error that prevented a code snippet from being
-- retrieved.
--
-- 'errorMessage', 'codeSnippetError_errorMessage' - The error message received when Amazon Inspector failed to retrieve a
-- code snippet.
--
-- 'findingArn', 'codeSnippetError_findingArn' - The ARN of the finding that a code snippet couldn\'t be retrieved for.
newCodeSnippetError ::
  -- | 'errorCode'
  CodeSnippetErrorCode ->
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'findingArn'
  Prelude.Text ->
  CodeSnippetError
newCodeSnippetError
  pErrorCode_
  pErrorMessage_
  pFindingArn_ =
    CodeSnippetError'
      { errorCode = pErrorCode_,
        errorMessage = pErrorMessage_,
        findingArn = pFindingArn_
      }

-- | The error code for the error that prevented a code snippet from being
-- retrieved.
codeSnippetError_errorCode :: Lens.Lens' CodeSnippetError CodeSnippetErrorCode
codeSnippetError_errorCode = Lens.lens (\CodeSnippetError' {errorCode} -> errorCode) (\s@CodeSnippetError' {} a -> s {errorCode = a} :: CodeSnippetError)

-- | The error message received when Amazon Inspector failed to retrieve a
-- code snippet.
codeSnippetError_errorMessage :: Lens.Lens' CodeSnippetError Prelude.Text
codeSnippetError_errorMessage = Lens.lens (\CodeSnippetError' {errorMessage} -> errorMessage) (\s@CodeSnippetError' {} a -> s {errorMessage = a} :: CodeSnippetError)

-- | The ARN of the finding that a code snippet couldn\'t be retrieved for.
codeSnippetError_findingArn :: Lens.Lens' CodeSnippetError Prelude.Text
codeSnippetError_findingArn = Lens.lens (\CodeSnippetError' {findingArn} -> findingArn) (\s@CodeSnippetError' {} a -> s {findingArn = a} :: CodeSnippetError)

instance Data.FromJSON CodeSnippetError where
  parseJSON =
    Data.withObject
      "CodeSnippetError"
      ( \x ->
          CodeSnippetError'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorMessage")
            Prelude.<*> (x Data..: "findingArn")
      )

instance Prelude.Hashable CodeSnippetError where
  hashWithSalt _salt CodeSnippetError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` findingArn

instance Prelude.NFData CodeSnippetError where
  rnf CodeSnippetError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf findingArn
