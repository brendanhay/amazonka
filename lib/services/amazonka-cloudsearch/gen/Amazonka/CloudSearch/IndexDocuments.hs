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
-- Module      : Amazonka.CloudSearch.IndexDocuments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tells the search domain to start indexing its documents using the latest
-- indexing options. This operation must be invoked to activate options
-- whose OptionStatus is @RequiresIndexDocuments@.
module Amazonka.CloudSearch.IndexDocuments
  ( -- * Creating a Request
    IndexDocuments (..),
    newIndexDocuments,

    -- * Request Lenses
    indexDocuments_domainName,

    -- * Destructuring the Response
    IndexDocumentsResponse (..),
    newIndexDocumentsResponse,

    -- * Response Lenses
    indexDocumentsResponse_fieldNames,
    indexDocumentsResponse_httpStatus,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @IndexDocuments@ operation.
-- Specifies the name of the domain you want to re-index.
--
-- /See:/ 'newIndexDocuments' smart constructor.
data IndexDocuments = IndexDocuments'
  { domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'indexDocuments_domainName' - Undocumented member.
newIndexDocuments ::
  -- | 'domainName'
  Prelude.Text ->
  IndexDocuments
newIndexDocuments pDomainName_ =
  IndexDocuments' {domainName = pDomainName_}

-- | Undocumented member.
indexDocuments_domainName :: Lens.Lens' IndexDocuments Prelude.Text
indexDocuments_domainName = Lens.lens (\IndexDocuments' {domainName} -> domainName) (\s@IndexDocuments' {} a -> s {domainName = a} :: IndexDocuments)

instance Core.AWSRequest IndexDocuments where
  type
    AWSResponse IndexDocuments =
      IndexDocumentsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "IndexDocumentsResult"
      ( \s h x ->
          IndexDocumentsResponse'
            Prelude.<$> ( x Core..@? "FieldNames" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable IndexDocuments where
  hashWithSalt _salt IndexDocuments' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData IndexDocuments where
  rnf IndexDocuments' {..} = Prelude.rnf domainName

instance Core.ToHeaders IndexDocuments where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath IndexDocuments where
  toPath = Prelude.const "/"

instance Core.ToQuery IndexDocuments where
  toQuery IndexDocuments' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("IndexDocuments" :: Prelude.ByteString),
        "Version"
          Core.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Core.=: domainName
      ]

-- | The result of an @IndexDocuments@ request. Contains the status of the
-- indexing operation, including the fields being indexed.
--
-- /See:/ 'newIndexDocumentsResponse' smart constructor.
data IndexDocumentsResponse = IndexDocumentsResponse'
  { -- | The names of the fields that are currently being indexed.
    fieldNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexDocumentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldNames', 'indexDocumentsResponse_fieldNames' - The names of the fields that are currently being indexed.
--
-- 'httpStatus', 'indexDocumentsResponse_httpStatus' - The response's http status code.
newIndexDocumentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  IndexDocumentsResponse
newIndexDocumentsResponse pHttpStatus_ =
  IndexDocumentsResponse'
    { fieldNames =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of the fields that are currently being indexed.
indexDocumentsResponse_fieldNames :: Lens.Lens' IndexDocumentsResponse (Prelude.Maybe [Prelude.Text])
indexDocumentsResponse_fieldNames = Lens.lens (\IndexDocumentsResponse' {fieldNames} -> fieldNames) (\s@IndexDocumentsResponse' {} a -> s {fieldNames = a} :: IndexDocumentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
indexDocumentsResponse_httpStatus :: Lens.Lens' IndexDocumentsResponse Prelude.Int
indexDocumentsResponse_httpStatus = Lens.lens (\IndexDocumentsResponse' {httpStatus} -> httpStatus) (\s@IndexDocumentsResponse' {} a -> s {httpStatus = a} :: IndexDocumentsResponse)

instance Prelude.NFData IndexDocumentsResponse where
  rnf IndexDocumentsResponse' {..} =
    Prelude.rnf fieldNames
      `Prelude.seq` Prelude.rnf httpStatus
