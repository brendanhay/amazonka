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
-- Module      : Amazonka.AccessAnalyzer.CreateAnalyzer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an analyzer for your account.
module Amazonka.AccessAnalyzer.CreateAnalyzer
  ( -- * Creating a Request
    CreateAnalyzer (..),
    newCreateAnalyzer,

    -- * Request Lenses
    createAnalyzer_tags,
    createAnalyzer_clientToken,
    createAnalyzer_archiveRules,
    createAnalyzer_analyzerName,
    createAnalyzer_type,

    -- * Destructuring the Response
    CreateAnalyzerResponse (..),
    newCreateAnalyzerResponse,

    -- * Response Lenses
    createAnalyzerResponse_arn,
    createAnalyzerResponse_httpStatus,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates an analyzer.
--
-- /See:/ 'newCreateAnalyzer' smart constructor.
data CreateAnalyzer = CreateAnalyzer'
  { -- | The tags to apply to the analyzer.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the archive rules to add for the analyzer. Archive rules
    -- automatically archive findings that meet the criteria you define for the
    -- rule.
    archiveRules :: Prelude.Maybe [InlineArchiveRule],
    -- | The name of the analyzer to create.
    analyzerName :: Prelude.Text,
    -- | The type of analyzer to create. Only ACCOUNT and ORGANIZATION analyzers
    -- are supported. You can create only one analyzer per account per Region.
    -- You can create up to 5 analyzers per organization per Region.
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnalyzer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAnalyzer_tags' - The tags to apply to the analyzer.
--
-- 'clientToken', 'createAnalyzer_clientToken' - A client token.
--
-- 'archiveRules', 'createAnalyzer_archiveRules' - Specifies the archive rules to add for the analyzer. Archive rules
-- automatically archive findings that meet the criteria you define for the
-- rule.
--
-- 'analyzerName', 'createAnalyzer_analyzerName' - The name of the analyzer to create.
--
-- 'type'', 'createAnalyzer_type' - The type of analyzer to create. Only ACCOUNT and ORGANIZATION analyzers
-- are supported. You can create only one analyzer per account per Region.
-- You can create up to 5 analyzers per organization per Region.
newCreateAnalyzer ::
  -- | 'analyzerName'
  Prelude.Text ->
  -- | 'type''
  Type ->
  CreateAnalyzer
newCreateAnalyzer pAnalyzerName_ pType_ =
  CreateAnalyzer'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      archiveRules = Prelude.Nothing,
      analyzerName = pAnalyzerName_,
      type' = pType_
    }

-- | The tags to apply to the analyzer.
createAnalyzer_tags :: Lens.Lens' CreateAnalyzer (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAnalyzer_tags = Lens.lens (\CreateAnalyzer' {tags} -> tags) (\s@CreateAnalyzer' {} a -> s {tags = a} :: CreateAnalyzer) Prelude.. Lens.mapping Lens.coerced

-- | A client token.
createAnalyzer_clientToken :: Lens.Lens' CreateAnalyzer (Prelude.Maybe Prelude.Text)
createAnalyzer_clientToken = Lens.lens (\CreateAnalyzer' {clientToken} -> clientToken) (\s@CreateAnalyzer' {} a -> s {clientToken = a} :: CreateAnalyzer)

-- | Specifies the archive rules to add for the analyzer. Archive rules
-- automatically archive findings that meet the criteria you define for the
-- rule.
createAnalyzer_archiveRules :: Lens.Lens' CreateAnalyzer (Prelude.Maybe [InlineArchiveRule])
createAnalyzer_archiveRules = Lens.lens (\CreateAnalyzer' {archiveRules} -> archiveRules) (\s@CreateAnalyzer' {} a -> s {archiveRules = a} :: CreateAnalyzer) Prelude.. Lens.mapping Lens.coerced

-- | The name of the analyzer to create.
createAnalyzer_analyzerName :: Lens.Lens' CreateAnalyzer Prelude.Text
createAnalyzer_analyzerName = Lens.lens (\CreateAnalyzer' {analyzerName} -> analyzerName) (\s@CreateAnalyzer' {} a -> s {analyzerName = a} :: CreateAnalyzer)

-- | The type of analyzer to create. Only ACCOUNT and ORGANIZATION analyzers
-- are supported. You can create only one analyzer per account per Region.
-- You can create up to 5 analyzers per organization per Region.
createAnalyzer_type :: Lens.Lens' CreateAnalyzer Type
createAnalyzer_type = Lens.lens (\CreateAnalyzer' {type'} -> type') (\s@CreateAnalyzer' {} a -> s {type' = a} :: CreateAnalyzer)

instance Core.AWSRequest CreateAnalyzer where
  type
    AWSResponse CreateAnalyzer =
      CreateAnalyzerResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAnalyzerResponse'
            Prelude.<$> (x Core..?> "arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAnalyzer where
  hashWithSalt _salt CreateAnalyzer' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` archiveRules
      `Prelude.hashWithSalt` analyzerName
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateAnalyzer where
  rnf CreateAnalyzer' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf archiveRules
      `Prelude.seq` Prelude.rnf analyzerName
      `Prelude.seq` Prelude.rnf type'

instance Core.ToHeaders CreateAnalyzer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateAnalyzer where
  toJSON CreateAnalyzer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("archiveRules" Core..=) Prelude.<$> archiveRules,
            Prelude.Just ("analyzerName" Core..= analyzerName),
            Prelude.Just ("type" Core..= type')
          ]
      )

instance Core.ToPath CreateAnalyzer where
  toPath = Prelude.const "/analyzer"

instance Core.ToQuery CreateAnalyzer where
  toQuery = Prelude.const Prelude.mempty

-- | The response to the request to create an analyzer.
--
-- /See:/ 'newCreateAnalyzerResponse' smart constructor.
data CreateAnalyzerResponse = CreateAnalyzerResponse'
  { -- | The ARN of the analyzer that was created by the request.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAnalyzerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createAnalyzerResponse_arn' - The ARN of the analyzer that was created by the request.
--
-- 'httpStatus', 'createAnalyzerResponse_httpStatus' - The response's http status code.
newCreateAnalyzerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAnalyzerResponse
newCreateAnalyzerResponse pHttpStatus_ =
  CreateAnalyzerResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the analyzer that was created by the request.
createAnalyzerResponse_arn :: Lens.Lens' CreateAnalyzerResponse (Prelude.Maybe Prelude.Text)
createAnalyzerResponse_arn = Lens.lens (\CreateAnalyzerResponse' {arn} -> arn) (\s@CreateAnalyzerResponse' {} a -> s {arn = a} :: CreateAnalyzerResponse)

-- | The response's http status code.
createAnalyzerResponse_httpStatus :: Lens.Lens' CreateAnalyzerResponse Prelude.Int
createAnalyzerResponse_httpStatus = Lens.lens (\CreateAnalyzerResponse' {httpStatus} -> httpStatus) (\s@CreateAnalyzerResponse' {} a -> s {httpStatus = a} :: CreateAnalyzerResponse)

instance Prelude.NFData CreateAnalyzerResponse where
  rnf CreateAnalyzerResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
