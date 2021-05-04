{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @IndexDocuments@ operation.
-- Specifies the name of the domain you want to re-index.
--
-- /See:/ 'newIndexDocuments' smart constructor.
data IndexDocuments = IndexDocuments'
  { domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest IndexDocuments where
  type Rs IndexDocuments = IndexDocumentsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "IndexDocumentsResult"
      ( \s h x ->
          IndexDocumentsResponse'
            Prelude.<$> ( x Prelude..@? "FieldNames"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable IndexDocuments

instance Prelude.NFData IndexDocuments

instance Prelude.ToHeaders IndexDocuments where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath IndexDocuments where
  toPath = Prelude.const "/"

instance Prelude.ToQuery IndexDocuments where
  toQuery IndexDocuments' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("IndexDocuments" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
indexDocumentsResponse_fieldNames = Lens.lens (\IndexDocumentsResponse' {fieldNames} -> fieldNames) (\s@IndexDocumentsResponse' {} a -> s {fieldNames = a} :: IndexDocumentsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
indexDocumentsResponse_httpStatus :: Lens.Lens' IndexDocumentsResponse Prelude.Int
indexDocumentsResponse_httpStatus = Lens.lens (\IndexDocumentsResponse' {httpStatus} -> httpStatus) (\s@IndexDocumentsResponse' {} a -> s {httpStatus = a} :: IndexDocumentsResponse)

instance Prelude.NFData IndexDocumentsResponse
