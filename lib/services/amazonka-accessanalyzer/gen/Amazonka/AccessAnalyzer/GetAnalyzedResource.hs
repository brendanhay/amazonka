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
-- Module      : Amazonka.AccessAnalyzer.GetAnalyzedResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource that was analyzed.
module Amazonka.AccessAnalyzer.GetAnalyzedResource
  ( -- * Creating a Request
    GetAnalyzedResource (..),
    newGetAnalyzedResource,

    -- * Request Lenses
    getAnalyzedResource_analyzerArn,
    getAnalyzedResource_resourceArn,

    -- * Destructuring the Response
    GetAnalyzedResourceResponse (..),
    newGetAnalyzedResourceResponse,

    -- * Response Lenses
    getAnalyzedResourceResponse_resource,
    getAnalyzedResourceResponse_httpStatus,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Retrieves an analyzed resource.
--
-- /See:/ 'newGetAnalyzedResource' smart constructor.
data GetAnalyzedResource = GetAnalyzedResource'
  { -- | The
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
    -- to retrieve information from.
    analyzerArn :: Prelude.Text,
    -- | The ARN of the resource to retrieve information about.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnalyzedResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyzerArn', 'getAnalyzedResource_analyzerArn' - The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
-- to retrieve information from.
--
-- 'resourceArn', 'getAnalyzedResource_resourceArn' - The ARN of the resource to retrieve information about.
newGetAnalyzedResource ::
  -- | 'analyzerArn'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  GetAnalyzedResource
newGetAnalyzedResource pAnalyzerArn_ pResourceArn_ =
  GetAnalyzedResource'
    { analyzerArn = pAnalyzerArn_,
      resourceArn = pResourceArn_
    }

-- | The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
-- to retrieve information from.
getAnalyzedResource_analyzerArn :: Lens.Lens' GetAnalyzedResource Prelude.Text
getAnalyzedResource_analyzerArn = Lens.lens (\GetAnalyzedResource' {analyzerArn} -> analyzerArn) (\s@GetAnalyzedResource' {} a -> s {analyzerArn = a} :: GetAnalyzedResource)

-- | The ARN of the resource to retrieve information about.
getAnalyzedResource_resourceArn :: Lens.Lens' GetAnalyzedResource Prelude.Text
getAnalyzedResource_resourceArn = Lens.lens (\GetAnalyzedResource' {resourceArn} -> resourceArn) (\s@GetAnalyzedResource' {} a -> s {resourceArn = a} :: GetAnalyzedResource)

instance Core.AWSRequest GetAnalyzedResource where
  type
    AWSResponse GetAnalyzedResource =
      GetAnalyzedResourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAnalyzedResourceResponse'
            Prelude.<$> (x Data..?> "resource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAnalyzedResource where
  hashWithSalt _salt GetAnalyzedResource' {..} =
    _salt
      `Prelude.hashWithSalt` analyzerArn
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetAnalyzedResource where
  rnf GetAnalyzedResource' {..} =
    Prelude.rnf analyzerArn `Prelude.seq`
      Prelude.rnf resourceArn

instance Data.ToHeaders GetAnalyzedResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAnalyzedResource where
  toPath = Prelude.const "/analyzed-resource"

instance Data.ToQuery GetAnalyzedResource where
  toQuery GetAnalyzedResource' {..} =
    Prelude.mconcat
      [ "analyzerArn" Data.=: analyzerArn,
        "resourceArn" Data.=: resourceArn
      ]

-- | The response to the request.
--
-- /See:/ 'newGetAnalyzedResourceResponse' smart constructor.
data GetAnalyzedResourceResponse = GetAnalyzedResourceResponse'
  { -- | An @AnalyzedResource@ object that contains information that IAM Access
    -- Analyzer found when it analyzed the resource.
    resource :: Prelude.Maybe AnalyzedResource,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAnalyzedResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resource', 'getAnalyzedResourceResponse_resource' - An @AnalyzedResource@ object that contains information that IAM Access
-- Analyzer found when it analyzed the resource.
--
-- 'httpStatus', 'getAnalyzedResourceResponse_httpStatus' - The response's http status code.
newGetAnalyzedResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAnalyzedResourceResponse
newGetAnalyzedResourceResponse pHttpStatus_ =
  GetAnalyzedResourceResponse'
    { resource =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An @AnalyzedResource@ object that contains information that IAM Access
-- Analyzer found when it analyzed the resource.
getAnalyzedResourceResponse_resource :: Lens.Lens' GetAnalyzedResourceResponse (Prelude.Maybe AnalyzedResource)
getAnalyzedResourceResponse_resource = Lens.lens (\GetAnalyzedResourceResponse' {resource} -> resource) (\s@GetAnalyzedResourceResponse' {} a -> s {resource = a} :: GetAnalyzedResourceResponse)

-- | The response's http status code.
getAnalyzedResourceResponse_httpStatus :: Lens.Lens' GetAnalyzedResourceResponse Prelude.Int
getAnalyzedResourceResponse_httpStatus = Lens.lens (\GetAnalyzedResourceResponse' {httpStatus} -> httpStatus) (\s@GetAnalyzedResourceResponse' {} a -> s {httpStatus = a} :: GetAnalyzedResourceResponse)

instance Prelude.NFData GetAnalyzedResourceResponse where
  rnf GetAnalyzedResourceResponse' {..} =
    Prelude.rnf resource `Prelude.seq`
      Prelude.rnf httpStatus
