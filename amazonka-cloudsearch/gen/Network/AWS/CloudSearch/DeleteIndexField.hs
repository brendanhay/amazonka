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
-- Module      : Network.AWS.CloudSearch.DeleteIndexField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an @IndexField@ from the search domain. For more information,
-- see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.DeleteIndexField
  ( -- * Creating a Request
    DeleteIndexField (..),
    newDeleteIndexField,

    -- * Request Lenses
    deleteIndexField_domainName,
    deleteIndexField_indexFieldName,

    -- * Destructuring the Response
    DeleteIndexFieldResponse (..),
    newDeleteIndexFieldResponse,

    -- * Response Lenses
    deleteIndexFieldResponse_httpStatus,
    deleteIndexFieldResponse_indexField,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DeleteIndexField@ operation.
-- Specifies the name of the domain you want to update and the name of the
-- index field you want to delete.
--
-- /See:/ 'newDeleteIndexField' smart constructor.
data DeleteIndexField = DeleteIndexField'
  { domainName :: Prelude.Text,
    -- | The name of the index field your want to remove from the domain\'s
    -- indexing options.
    indexFieldName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIndexField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteIndexField_domainName' - Undocumented member.
--
-- 'indexFieldName', 'deleteIndexField_indexFieldName' - The name of the index field your want to remove from the domain\'s
-- indexing options.
newDeleteIndexField ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'indexFieldName'
  Prelude.Text ->
  DeleteIndexField
newDeleteIndexField pDomainName_ pIndexFieldName_ =
  DeleteIndexField'
    { domainName = pDomainName_,
      indexFieldName = pIndexFieldName_
    }

-- | Undocumented member.
deleteIndexField_domainName :: Lens.Lens' DeleteIndexField Prelude.Text
deleteIndexField_domainName = Lens.lens (\DeleteIndexField' {domainName} -> domainName) (\s@DeleteIndexField' {} a -> s {domainName = a} :: DeleteIndexField)

-- | The name of the index field your want to remove from the domain\'s
-- indexing options.
deleteIndexField_indexFieldName :: Lens.Lens' DeleteIndexField Prelude.Text
deleteIndexField_indexFieldName = Lens.lens (\DeleteIndexField' {indexFieldName} -> indexFieldName) (\s@DeleteIndexField' {} a -> s {indexFieldName = a} :: DeleteIndexField)

instance Prelude.AWSRequest DeleteIndexField where
  type Rs DeleteIndexField = DeleteIndexFieldResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteIndexFieldResult"
      ( \s h x ->
          DeleteIndexFieldResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "IndexField")
      )

instance Prelude.Hashable DeleteIndexField

instance Prelude.NFData DeleteIndexField

instance Prelude.ToHeaders DeleteIndexField where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteIndexField where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteIndexField where
  toQuery DeleteIndexField' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteIndexField" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName,
        "IndexFieldName" Prelude.=: indexFieldName
      ]

-- | The result of a @DeleteIndexField@ request.
--
-- /See:/ 'newDeleteIndexFieldResponse' smart constructor.
data DeleteIndexFieldResponse = DeleteIndexFieldResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the index field being deleted.
    indexField :: IndexFieldStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIndexFieldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIndexFieldResponse_httpStatus' - The response's http status code.
--
-- 'indexField', 'deleteIndexFieldResponse_indexField' - The status of the index field being deleted.
newDeleteIndexFieldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'indexField'
  IndexFieldStatus ->
  DeleteIndexFieldResponse
newDeleteIndexFieldResponse pHttpStatus_ pIndexField_ =
  DeleteIndexFieldResponse'
    { httpStatus =
        pHttpStatus_,
      indexField = pIndexField_
    }

-- | The response's http status code.
deleteIndexFieldResponse_httpStatus :: Lens.Lens' DeleteIndexFieldResponse Prelude.Int
deleteIndexFieldResponse_httpStatus = Lens.lens (\DeleteIndexFieldResponse' {httpStatus} -> httpStatus) (\s@DeleteIndexFieldResponse' {} a -> s {httpStatus = a} :: DeleteIndexFieldResponse)

-- | The status of the index field being deleted.
deleteIndexFieldResponse_indexField :: Lens.Lens' DeleteIndexFieldResponse IndexFieldStatus
deleteIndexFieldResponse_indexField = Lens.lens (\DeleteIndexFieldResponse' {indexField} -> indexField) (\s@DeleteIndexFieldResponse' {} a -> s {indexField = a} :: DeleteIndexFieldResponse)

instance Prelude.NFData DeleteIndexFieldResponse
