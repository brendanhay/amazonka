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
-- Module      : Amazonka.CloudSearch.DescribeAnalysisSchemes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the analysis schemes configured for a domain. An analysis scheme
-- defines language-specific text processing options for a @text@ field.
-- Can be limited to specific analysis schemes by name. By default, shows
-- all analysis schemes and includes any pending changes to the
-- configuration. Set the @Deployed@ option to @true@ to show the active
-- configuration and exclude pending changes. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DescribeAnalysisSchemes
  ( -- * Creating a Request
    DescribeAnalysisSchemes (..),
    newDescribeAnalysisSchemes,

    -- * Request Lenses
    describeAnalysisSchemes_deployed,
    describeAnalysisSchemes_analysisSchemeNames,
    describeAnalysisSchemes_domainName,

    -- * Destructuring the Response
    DescribeAnalysisSchemesResponse (..),
    newDescribeAnalysisSchemesResponse,

    -- * Response Lenses
    describeAnalysisSchemesResponse_httpStatus,
    describeAnalysisSchemesResponse_analysisSchemes,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeAnalysisSchemes@ operation.
-- Specifies the name of the domain you want to describe. To limit the
-- response to particular analysis schemes, specify the names of the
-- analysis schemes you want to describe. To show the active configuration
-- and exclude any pending changes, set the @Deployed@ option to @true@.
--
-- /See:/ 'newDescribeAnalysisSchemes' smart constructor.
data DescribeAnalysisSchemes = DescribeAnalysisSchemes'
  { -- | Whether to display the deployed configuration (@true@) or include any
    -- pending changes (@false@). Defaults to @false@.
    deployed :: Prelude.Maybe Prelude.Bool,
    -- | The analysis schemes you want to describe.
    analysisSchemeNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the domain you want to describe.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnalysisSchemes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployed', 'describeAnalysisSchemes_deployed' - Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
--
-- 'analysisSchemeNames', 'describeAnalysisSchemes_analysisSchemeNames' - The analysis schemes you want to describe.
--
-- 'domainName', 'describeAnalysisSchemes_domainName' - The name of the domain you want to describe.
newDescribeAnalysisSchemes ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeAnalysisSchemes
newDescribeAnalysisSchemes pDomainName_ =
  DescribeAnalysisSchemes'
    { deployed =
        Prelude.Nothing,
      analysisSchemeNames = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeAnalysisSchemes_deployed :: Lens.Lens' DescribeAnalysisSchemes (Prelude.Maybe Prelude.Bool)
describeAnalysisSchemes_deployed = Lens.lens (\DescribeAnalysisSchemes' {deployed} -> deployed) (\s@DescribeAnalysisSchemes' {} a -> s {deployed = a} :: DescribeAnalysisSchemes)

-- | The analysis schemes you want to describe.
describeAnalysisSchemes_analysisSchemeNames :: Lens.Lens' DescribeAnalysisSchemes (Prelude.Maybe [Prelude.Text])
describeAnalysisSchemes_analysisSchemeNames = Lens.lens (\DescribeAnalysisSchemes' {analysisSchemeNames} -> analysisSchemeNames) (\s@DescribeAnalysisSchemes' {} a -> s {analysisSchemeNames = a} :: DescribeAnalysisSchemes) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain you want to describe.
describeAnalysisSchemes_domainName :: Lens.Lens' DescribeAnalysisSchemes Prelude.Text
describeAnalysisSchemes_domainName = Lens.lens (\DescribeAnalysisSchemes' {domainName} -> domainName) (\s@DescribeAnalysisSchemes' {} a -> s {domainName = a} :: DescribeAnalysisSchemes)

instance Core.AWSRequest DescribeAnalysisSchemes where
  type
    AWSResponse DescribeAnalysisSchemes =
      DescribeAnalysisSchemesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAnalysisSchemesResult"
      ( \s h x ->
          DescribeAnalysisSchemesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "AnalysisSchemes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeAnalysisSchemes where
  hashWithSalt _salt DescribeAnalysisSchemes' {..} =
    _salt `Prelude.hashWithSalt` deployed
      `Prelude.hashWithSalt` analysisSchemeNames
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeAnalysisSchemes where
  rnf DescribeAnalysisSchemes' {..} =
    Prelude.rnf deployed
      `Prelude.seq` Prelude.rnf analysisSchemeNames
      `Prelude.seq` Prelude.rnf domainName

instance Core.ToHeaders DescribeAnalysisSchemes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAnalysisSchemes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAnalysisSchemes where
  toQuery DescribeAnalysisSchemes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeAnalysisSchemes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2013-01-01" :: Prelude.ByteString),
        "Deployed" Core.=: deployed,
        "AnalysisSchemeNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> analysisSchemeNames
            ),
        "DomainName" Core.=: domainName
      ]

-- | The result of a @DescribeAnalysisSchemes@ request. Contains the analysis
-- schemes configured for the domain specified in the request.
--
-- /See:/ 'newDescribeAnalysisSchemesResponse' smart constructor.
data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The analysis scheme descriptions.
    analysisSchemes :: [AnalysisSchemeStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAnalysisSchemesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeAnalysisSchemesResponse_httpStatus' - The response's http status code.
--
-- 'analysisSchemes', 'describeAnalysisSchemesResponse_analysisSchemes' - The analysis scheme descriptions.
newDescribeAnalysisSchemesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAnalysisSchemesResponse
newDescribeAnalysisSchemesResponse pHttpStatus_ =
  DescribeAnalysisSchemesResponse'
    { httpStatus =
        pHttpStatus_,
      analysisSchemes = Prelude.mempty
    }

-- | The response's http status code.
describeAnalysisSchemesResponse_httpStatus :: Lens.Lens' DescribeAnalysisSchemesResponse Prelude.Int
describeAnalysisSchemesResponse_httpStatus = Lens.lens (\DescribeAnalysisSchemesResponse' {httpStatus} -> httpStatus) (\s@DescribeAnalysisSchemesResponse' {} a -> s {httpStatus = a} :: DescribeAnalysisSchemesResponse)

-- | The analysis scheme descriptions.
describeAnalysisSchemesResponse_analysisSchemes :: Lens.Lens' DescribeAnalysisSchemesResponse [AnalysisSchemeStatus]
describeAnalysisSchemesResponse_analysisSchemes = Lens.lens (\DescribeAnalysisSchemesResponse' {analysisSchemes} -> analysisSchemes) (\s@DescribeAnalysisSchemesResponse' {} a -> s {analysisSchemes = a} :: DescribeAnalysisSchemesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeAnalysisSchemesResponse
  where
  rnf DescribeAnalysisSchemesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf analysisSchemes
