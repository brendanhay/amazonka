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
-- Module      : Network.AWS.CloudSearch.IndexDocuments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tells the search domain to start indexing its documents using the latest
-- indexing options. This operation must be invoked to activate options
-- whose OptionStatus is @RequiresIndexDocuments@.
module Network.AWS.CloudSearch.IndexDocuments
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

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @IndexDocuments@ operation.
-- Specifies the name of the domain you want to re-index.
--
-- /See:/ 'newIndexDocuments' smart constructor.
data IndexDocuments = IndexDocuments'
  { domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  IndexDocuments
newIndexDocuments pDomainName_ =
  IndexDocuments' {domainName = pDomainName_}

-- | Undocumented member.
indexDocuments_domainName :: Lens.Lens' IndexDocuments Core.Text
indexDocuments_domainName = Lens.lens (\IndexDocuments' {domainName} -> domainName) (\s@IndexDocuments' {} a -> s {domainName = a} :: IndexDocuments)

instance Core.AWSRequest IndexDocuments where
  type
    AWSResponse IndexDocuments =
      IndexDocumentsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "IndexDocumentsResult"
      ( \s h x ->
          IndexDocumentsResponse'
            Core.<$> ( x Core..@? "FieldNames" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable IndexDocuments

instance Core.NFData IndexDocuments

instance Core.ToHeaders IndexDocuments where
  toHeaders = Core.const Core.mempty

instance Core.ToPath IndexDocuments where
  toPath = Core.const "/"

instance Core.ToQuery IndexDocuments where
  toQuery IndexDocuments' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("IndexDocuments" :: Core.ByteString),
        "Version" Core.=: ("2013-01-01" :: Core.ByteString),
        "DomainName" Core.=: domainName
      ]

-- | The result of an @IndexDocuments@ request. Contains the status of the
-- indexing operation, including the fields being indexed.
--
-- /See:/ 'newIndexDocumentsResponse' smart constructor.
data IndexDocumentsResponse = IndexDocumentsResponse'
  { -- | The names of the fields that are currently being indexed.
    fieldNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  IndexDocumentsResponse
newIndexDocumentsResponse pHttpStatus_ =
  IndexDocumentsResponse'
    { fieldNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of the fields that are currently being indexed.
indexDocumentsResponse_fieldNames :: Lens.Lens' IndexDocumentsResponse (Core.Maybe [Core.Text])
indexDocumentsResponse_fieldNames = Lens.lens (\IndexDocumentsResponse' {fieldNames} -> fieldNames) (\s@IndexDocumentsResponse' {} a -> s {fieldNames = a} :: IndexDocumentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
indexDocumentsResponse_httpStatus :: Lens.Lens' IndexDocumentsResponse Core.Int
indexDocumentsResponse_httpStatus = Lens.lens (\IndexDocumentsResponse' {httpStatus} -> httpStatus) (\s@IndexDocumentsResponse' {} a -> s {httpStatus = a} :: IndexDocumentsResponse)

instance Core.NFData IndexDocumentsResponse
