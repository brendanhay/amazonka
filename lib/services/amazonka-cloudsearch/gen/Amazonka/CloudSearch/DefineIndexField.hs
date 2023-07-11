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
-- Module      : Amazonka.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an @IndexField@ for the search domain. Used to create new
-- fields and modify existing ones. You must specify the name of the domain
-- you are configuring and an index field configuration. The index field
-- configuration specifies a unique name, the index field type, and the
-- options you want to configure for the field. The options you can specify
-- depend on the @IndexFieldType@. If the field exists, the new
-- configuration replaces the old one. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DefineIndexField
  ( -- * Creating a Request
    DefineIndexField (..),
    newDefineIndexField,

    -- * Request Lenses
    defineIndexField_domainName,
    defineIndexField_indexField,

    -- * Destructuring the Response
    DefineIndexFieldResponse (..),
    newDefineIndexFieldResponse,

    -- * Response Lenses
    defineIndexFieldResponse_httpStatus,
    defineIndexFieldResponse_indexField,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DefineIndexField@ operation.
-- Specifies the name of the domain you want to update and the index field
-- configuration.
--
-- /See:/ 'newDefineIndexField' smart constructor.
data DefineIndexField = DefineIndexField'
  { domainName :: Prelude.Text,
    -- | The index field and field options you want to configure.
    indexField :: IndexField
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefineIndexField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'defineIndexField_domainName' - Undocumented member.
--
-- 'indexField', 'defineIndexField_indexField' - The index field and field options you want to configure.
newDefineIndexField ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'indexField'
  IndexField ->
  DefineIndexField
newDefineIndexField pDomainName_ pIndexField_ =
  DefineIndexField'
    { domainName = pDomainName_,
      indexField = pIndexField_
    }

-- | Undocumented member.
defineIndexField_domainName :: Lens.Lens' DefineIndexField Prelude.Text
defineIndexField_domainName = Lens.lens (\DefineIndexField' {domainName} -> domainName) (\s@DefineIndexField' {} a -> s {domainName = a} :: DefineIndexField)

-- | The index field and field options you want to configure.
defineIndexField_indexField :: Lens.Lens' DefineIndexField IndexField
defineIndexField_indexField = Lens.lens (\DefineIndexField' {indexField} -> indexField) (\s@DefineIndexField' {} a -> s {indexField = a} :: DefineIndexField)

instance Core.AWSRequest DefineIndexField where
  type
    AWSResponse DefineIndexField =
      DefineIndexFieldResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DefineIndexFieldResult"
      ( \s h x ->
          DefineIndexFieldResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "IndexField")
      )

instance Prelude.Hashable DefineIndexField where
  hashWithSalt _salt DefineIndexField' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` indexField

instance Prelude.NFData DefineIndexField where
  rnf DefineIndexField' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf indexField

instance Data.ToHeaders DefineIndexField where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DefineIndexField where
  toPath = Prelude.const "/"

instance Data.ToQuery DefineIndexField where
  toQuery DefineIndexField' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DefineIndexField" :: Prelude.ByteString),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Data.=: domainName,
        "IndexField" Data.=: indexField
      ]

-- | The result of a @DefineIndexField@ request. Contains the status of the
-- newly-configured index field.
--
-- /See:/ 'newDefineIndexFieldResponse' smart constructor.
data DefineIndexFieldResponse = DefineIndexFieldResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    indexField :: IndexFieldStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefineIndexFieldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'defineIndexFieldResponse_httpStatus' - The response's http status code.
--
-- 'indexField', 'defineIndexFieldResponse_indexField' - Undocumented member.
newDefineIndexFieldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'indexField'
  IndexFieldStatus ->
  DefineIndexFieldResponse
newDefineIndexFieldResponse pHttpStatus_ pIndexField_ =
  DefineIndexFieldResponse'
    { httpStatus =
        pHttpStatus_,
      indexField = pIndexField_
    }

-- | The response's http status code.
defineIndexFieldResponse_httpStatus :: Lens.Lens' DefineIndexFieldResponse Prelude.Int
defineIndexFieldResponse_httpStatus = Lens.lens (\DefineIndexFieldResponse' {httpStatus} -> httpStatus) (\s@DefineIndexFieldResponse' {} a -> s {httpStatus = a} :: DefineIndexFieldResponse)

-- | Undocumented member.
defineIndexFieldResponse_indexField :: Lens.Lens' DefineIndexFieldResponse IndexFieldStatus
defineIndexFieldResponse_indexField = Lens.lens (\DefineIndexFieldResponse' {indexField} -> indexField) (\s@DefineIndexFieldResponse' {} a -> s {indexField = a} :: DefineIndexFieldResponse)

instance Prelude.NFData DefineIndexFieldResponse where
  rnf DefineIndexFieldResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf indexField
