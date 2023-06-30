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
-- Module      : Amazonka.CloudSearch.DeleteAnalysisScheme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analysis scheme. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-analysis-schemes.html Configuring Analysis Schemes>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DeleteAnalysisScheme
  ( -- * Creating a Request
    DeleteAnalysisScheme (..),
    newDeleteAnalysisScheme,

    -- * Request Lenses
    deleteAnalysisScheme_domainName,
    deleteAnalysisScheme_analysisSchemeName,

    -- * Destructuring the Response
    DeleteAnalysisSchemeResponse (..),
    newDeleteAnalysisSchemeResponse,

    -- * Response Lenses
    deleteAnalysisSchemeResponse_httpStatus,
    deleteAnalysisSchemeResponse_analysisScheme,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DeleteAnalysisScheme@ operation.
-- Specifies the name of the domain you want to update and the analysis
-- scheme you want to delete.
--
-- /See:/ 'newDeleteAnalysisScheme' smart constructor.
data DeleteAnalysisScheme = DeleteAnalysisScheme'
  { domainName :: Prelude.Text,
    -- | The name of the analysis scheme you want to delete.
    analysisSchemeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnalysisScheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteAnalysisScheme_domainName' - Undocumented member.
--
-- 'analysisSchemeName', 'deleteAnalysisScheme_analysisSchemeName' - The name of the analysis scheme you want to delete.
newDeleteAnalysisScheme ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'analysisSchemeName'
  Prelude.Text ->
  DeleteAnalysisScheme
newDeleteAnalysisScheme
  pDomainName_
  pAnalysisSchemeName_ =
    DeleteAnalysisScheme'
      { domainName = pDomainName_,
        analysisSchemeName = pAnalysisSchemeName_
      }

-- | Undocumented member.
deleteAnalysisScheme_domainName :: Lens.Lens' DeleteAnalysisScheme Prelude.Text
deleteAnalysisScheme_domainName = Lens.lens (\DeleteAnalysisScheme' {domainName} -> domainName) (\s@DeleteAnalysisScheme' {} a -> s {domainName = a} :: DeleteAnalysisScheme)

-- | The name of the analysis scheme you want to delete.
deleteAnalysisScheme_analysisSchemeName :: Lens.Lens' DeleteAnalysisScheme Prelude.Text
deleteAnalysisScheme_analysisSchemeName = Lens.lens (\DeleteAnalysisScheme' {analysisSchemeName} -> analysisSchemeName) (\s@DeleteAnalysisScheme' {} a -> s {analysisSchemeName = a} :: DeleteAnalysisScheme)

instance Core.AWSRequest DeleteAnalysisScheme where
  type
    AWSResponse DeleteAnalysisScheme =
      DeleteAnalysisSchemeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteAnalysisSchemeResult"
      ( \s h x ->
          DeleteAnalysisSchemeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "AnalysisScheme")
      )

instance Prelude.Hashable DeleteAnalysisScheme where
  hashWithSalt _salt DeleteAnalysisScheme' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` analysisSchemeName

instance Prelude.NFData DeleteAnalysisScheme where
  rnf DeleteAnalysisScheme' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf analysisSchemeName

instance Data.ToHeaders DeleteAnalysisScheme where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteAnalysisScheme where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAnalysisScheme where
  toQuery DeleteAnalysisScheme' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteAnalysisScheme" :: Prelude.ByteString),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Data.=: domainName,
        "AnalysisSchemeName" Data.=: analysisSchemeName
      ]

-- | The result of a @DeleteAnalysisScheme@ request. Contains the status of
-- the deleted analysis scheme.
--
-- /See:/ 'newDeleteAnalysisSchemeResponse' smart constructor.
data DeleteAnalysisSchemeResponse = DeleteAnalysisSchemeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the analysis scheme being deleted.
    analysisScheme :: AnalysisSchemeStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAnalysisSchemeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAnalysisSchemeResponse_httpStatus' - The response's http status code.
--
-- 'analysisScheme', 'deleteAnalysisSchemeResponse_analysisScheme' - The status of the analysis scheme being deleted.
newDeleteAnalysisSchemeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'analysisScheme'
  AnalysisSchemeStatus ->
  DeleteAnalysisSchemeResponse
newDeleteAnalysisSchemeResponse
  pHttpStatus_
  pAnalysisScheme_ =
    DeleteAnalysisSchemeResponse'
      { httpStatus =
          pHttpStatus_,
        analysisScheme = pAnalysisScheme_
      }

-- | The response's http status code.
deleteAnalysisSchemeResponse_httpStatus :: Lens.Lens' DeleteAnalysisSchemeResponse Prelude.Int
deleteAnalysisSchemeResponse_httpStatus = Lens.lens (\DeleteAnalysisSchemeResponse' {httpStatus} -> httpStatus) (\s@DeleteAnalysisSchemeResponse' {} a -> s {httpStatus = a} :: DeleteAnalysisSchemeResponse)

-- | The status of the analysis scheme being deleted.
deleteAnalysisSchemeResponse_analysisScheme :: Lens.Lens' DeleteAnalysisSchemeResponse AnalysisSchemeStatus
deleteAnalysisSchemeResponse_analysisScheme = Lens.lens (\DeleteAnalysisSchemeResponse' {analysisScheme} -> analysisScheme) (\s@DeleteAnalysisSchemeResponse' {} a -> s {analysisScheme = a} :: DeleteAnalysisSchemeResponse)

instance Prelude.NFData DeleteAnalysisSchemeResponse where
  rnf DeleteAnalysisSchemeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf analysisScheme
