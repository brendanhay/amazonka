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
-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.CloudSearch.DefineIndexField
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

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DefineIndexField@ operation.
-- Specifies the name of the domain you want to update and the index field
-- configuration.
--
-- /See:/ 'newDefineIndexField' smart constructor.
data DefineIndexField = DefineIndexField'
  { domainName :: Core.Text,
    -- | The index field and field options you want to configure.
    indexField :: IndexField
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'indexField'
  IndexField ->
  DefineIndexField
newDefineIndexField pDomainName_ pIndexField_ =
  DefineIndexField'
    { domainName = pDomainName_,
      indexField = pIndexField_
    }

-- | Undocumented member.
defineIndexField_domainName :: Lens.Lens' DefineIndexField Core.Text
defineIndexField_domainName = Lens.lens (\DefineIndexField' {domainName} -> domainName) (\s@DefineIndexField' {} a -> s {domainName = a} :: DefineIndexField)

-- | The index field and field options you want to configure.
defineIndexField_indexField :: Lens.Lens' DefineIndexField IndexField
defineIndexField_indexField = Lens.lens (\DefineIndexField' {indexField} -> indexField) (\s@DefineIndexField' {} a -> s {indexField = a} :: DefineIndexField)

instance Core.AWSRequest DefineIndexField where
  type
    AWSResponse DefineIndexField =
      DefineIndexFieldResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DefineIndexFieldResult"
      ( \s h x ->
          DefineIndexFieldResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "IndexField")
      )

instance Core.Hashable DefineIndexField

instance Core.NFData DefineIndexField

instance Core.ToHeaders DefineIndexField where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DefineIndexField where
  toPath = Core.const "/"

instance Core.ToQuery DefineIndexField where
  toQuery DefineIndexField' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DefineIndexField" :: Core.ByteString),
        "Version" Core.=: ("2013-01-01" :: Core.ByteString),
        "DomainName" Core.=: domainName,
        "IndexField" Core.=: indexField
      ]

-- | The result of a @DefineIndexField@ request. Contains the status of the
-- newly-configured index field.
--
-- /See:/ 'newDefineIndexFieldResponse' smart constructor.
data DefineIndexFieldResponse = DefineIndexFieldResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    indexField :: IndexFieldStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
defineIndexFieldResponse_httpStatus :: Lens.Lens' DefineIndexFieldResponse Core.Int
defineIndexFieldResponse_httpStatus = Lens.lens (\DefineIndexFieldResponse' {httpStatus} -> httpStatus) (\s@DefineIndexFieldResponse' {} a -> s {httpStatus = a} :: DefineIndexFieldResponse)

-- | Undocumented member.
defineIndexFieldResponse_indexField :: Lens.Lens' DefineIndexFieldResponse IndexFieldStatus
defineIndexFieldResponse_indexField = Lens.lens (\DefineIndexFieldResponse' {indexField} -> indexField) (\s@DefineIndexFieldResponse' {} a -> s {indexField = a} :: DefineIndexFieldResponse)

instance Core.NFData DefineIndexFieldResponse
