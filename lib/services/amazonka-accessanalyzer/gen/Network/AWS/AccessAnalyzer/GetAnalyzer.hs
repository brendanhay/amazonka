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
-- Module      : Network.AWS.AccessAnalyzer.GetAnalyzer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified analyzer.
module Network.AWS.AccessAnalyzer.GetAnalyzer
  ( -- * Creating a Request
    GetAnalyzer (..),
    newGetAnalyzer,

    -- * Request Lenses
    getAnalyzer_analyzerName,

    -- * Destructuring the Response
    GetAnalyzerResponse (..),
    newGetAnalyzerResponse,

    -- * Response Lenses
    getAnalyzerResponse_httpStatus,
    getAnalyzerResponse_analyzer,
  )
where

import Network.AWS.AccessAnalyzer.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Retrieves an analyzer.
--
-- /See:/ 'newGetAnalyzer' smart constructor.
data GetAnalyzer = GetAnalyzer'
  { -- | The name of the analyzer retrieved.
    analyzerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnalyzer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzerName', 'getAnalyzer_analyzerName' - The name of the analyzer retrieved.
newGetAnalyzer ::
  -- | 'analyzerName'
  Prelude.Text ->
  GetAnalyzer
newGetAnalyzer pAnalyzerName_ =
  GetAnalyzer' {analyzerName = pAnalyzerName_}

-- | The name of the analyzer retrieved.
getAnalyzer_analyzerName :: Lens.Lens' GetAnalyzer Prelude.Text
getAnalyzer_analyzerName = Lens.lens (\GetAnalyzer' {analyzerName} -> analyzerName) (\s@GetAnalyzer' {} a -> s {analyzerName = a} :: GetAnalyzer)

instance Core.AWSRequest GetAnalyzer where
  type AWSResponse GetAnalyzer = GetAnalyzerResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnalyzerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "analyzer")
      )

instance Prelude.Hashable GetAnalyzer

instance Prelude.NFData GetAnalyzer

instance Core.ToHeaders GetAnalyzer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAnalyzer where
  toPath GetAnalyzer' {..} =
    Prelude.mconcat
      ["/analyzer/", Core.toBS analyzerName]

instance Core.ToQuery GetAnalyzer where
  toQuery = Prelude.const Prelude.mempty

-- | The response to the request.
--
-- /See:/ 'newGetAnalyzerResponse' smart constructor.
data GetAnalyzerResponse = GetAnalyzerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An @AnalyzerSummary@ object that contains information about the
    -- analyzer.
    analyzer :: AnalyzerSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnalyzerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAnalyzerResponse_httpStatus' - The response's http status code.
--
-- 'analyzer', 'getAnalyzerResponse_analyzer' - An @AnalyzerSummary@ object that contains information about the
-- analyzer.
newGetAnalyzerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'analyzer'
  AnalyzerSummary ->
  GetAnalyzerResponse
newGetAnalyzerResponse pHttpStatus_ pAnalyzer_ =
  GetAnalyzerResponse'
    { httpStatus = pHttpStatus_,
      analyzer = pAnalyzer_
    }

-- | The response's http status code.
getAnalyzerResponse_httpStatus :: Lens.Lens' GetAnalyzerResponse Prelude.Int
getAnalyzerResponse_httpStatus = Lens.lens (\GetAnalyzerResponse' {httpStatus} -> httpStatus) (\s@GetAnalyzerResponse' {} a -> s {httpStatus = a} :: GetAnalyzerResponse)

-- | An @AnalyzerSummary@ object that contains information about the
-- analyzer.
getAnalyzerResponse_analyzer :: Lens.Lens' GetAnalyzerResponse AnalyzerSummary
getAnalyzerResponse_analyzer = Lens.lens (\GetAnalyzerResponse' {analyzer} -> analyzer) (\s@GetAnalyzerResponse' {} a -> s {analyzer = a} :: GetAnalyzerResponse)

instance Prelude.NFData GetAnalyzerResponse
