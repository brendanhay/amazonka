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
-- Module      : Network.AWS.CloudSearch.DescribeAnalysisSchemes
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CloudSearch.DescribeAnalysisSchemes
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

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    deployed :: Core.Maybe Core.Bool,
    -- | The analysis schemes you want to describe.
    analysisSchemeNames :: Core.Maybe [Core.Text],
    -- | The name of the domain you want to describe.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeAnalysisSchemes
newDescribeAnalysisSchemes pDomainName_ =
  DescribeAnalysisSchemes'
    { deployed = Core.Nothing,
      analysisSchemeNames = Core.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeAnalysisSchemes_deployed :: Lens.Lens' DescribeAnalysisSchemes (Core.Maybe Core.Bool)
describeAnalysisSchemes_deployed = Lens.lens (\DescribeAnalysisSchemes' {deployed} -> deployed) (\s@DescribeAnalysisSchemes' {} a -> s {deployed = a} :: DescribeAnalysisSchemes)

-- | The analysis schemes you want to describe.
describeAnalysisSchemes_analysisSchemeNames :: Lens.Lens' DescribeAnalysisSchemes (Core.Maybe [Core.Text])
describeAnalysisSchemes_analysisSchemeNames = Lens.lens (\DescribeAnalysisSchemes' {analysisSchemeNames} -> analysisSchemeNames) (\s@DescribeAnalysisSchemes' {} a -> s {analysisSchemeNames = a} :: DescribeAnalysisSchemes) Core.. Lens.mapping Lens._Coerce

-- | The name of the domain you want to describe.
describeAnalysisSchemes_domainName :: Lens.Lens' DescribeAnalysisSchemes Core.Text
describeAnalysisSchemes_domainName = Lens.lens (\DescribeAnalysisSchemes' {domainName} -> domainName) (\s@DescribeAnalysisSchemes' {} a -> s {domainName = a} :: DescribeAnalysisSchemes)

instance Core.AWSRequest DescribeAnalysisSchemes where
  type
    AWSResponse DescribeAnalysisSchemes =
      DescribeAnalysisSchemesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAnalysisSchemesResult"
      ( \s h x ->
          DescribeAnalysisSchemesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "AnalysisSchemes" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable DescribeAnalysisSchemes

instance Core.NFData DescribeAnalysisSchemes

instance Core.ToHeaders DescribeAnalysisSchemes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAnalysisSchemes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAnalysisSchemes where
  toQuery DescribeAnalysisSchemes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeAnalysisSchemes" :: Core.ByteString),
        "Version" Core.=: ("2013-01-01" :: Core.ByteString),
        "Deployed" Core.=: deployed,
        "AnalysisSchemeNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> analysisSchemeNames
            ),
        "DomainName" Core.=: domainName
      ]

-- | The result of a @DescribeAnalysisSchemes@ request. Contains the analysis
-- schemes configured for the domain specified in the request.
--
-- /See:/ 'newDescribeAnalysisSchemesResponse' smart constructor.
data DescribeAnalysisSchemesResponse = DescribeAnalysisSchemesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The analysis scheme descriptions.
    analysisSchemes :: [AnalysisSchemeStatus]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeAnalysisSchemesResponse
newDescribeAnalysisSchemesResponse pHttpStatus_ =
  DescribeAnalysisSchemesResponse'
    { httpStatus =
        pHttpStatus_,
      analysisSchemes = Core.mempty
    }

-- | The response's http status code.
describeAnalysisSchemesResponse_httpStatus :: Lens.Lens' DescribeAnalysisSchemesResponse Core.Int
describeAnalysisSchemesResponse_httpStatus = Lens.lens (\DescribeAnalysisSchemesResponse' {httpStatus} -> httpStatus) (\s@DescribeAnalysisSchemesResponse' {} a -> s {httpStatus = a} :: DescribeAnalysisSchemesResponse)

-- | The analysis scheme descriptions.
describeAnalysisSchemesResponse_analysisSchemes :: Lens.Lens' DescribeAnalysisSchemesResponse [AnalysisSchemeStatus]
describeAnalysisSchemesResponse_analysisSchemes = Lens.lens (\DescribeAnalysisSchemesResponse' {analysisSchemes} -> analysisSchemes) (\s@DescribeAnalysisSchemesResponse' {} a -> s {analysisSchemes = a} :: DescribeAnalysisSchemesResponse) Core.. Lens._Coerce

instance Core.NFData DescribeAnalysisSchemesResponse
