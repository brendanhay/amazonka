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
-- Module      : Amazonka.CustomerProfiles.DeleteCalculatedAttributeDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing calculated attribute definition. Note that deleting
-- a default calculated attribute is possible, however once deleted, you
-- will be unable to undo that action and will need to recreate it on your
-- own using the CreateCalculatedAttributeDefinition API if you want it
-- back.
module Amazonka.CustomerProfiles.DeleteCalculatedAttributeDefinition
  ( -- * Creating a Request
    DeleteCalculatedAttributeDefinition (..),
    newDeleteCalculatedAttributeDefinition,

    -- * Request Lenses
    deleteCalculatedAttributeDefinition_domainName,
    deleteCalculatedAttributeDefinition_calculatedAttributeName,

    -- * Destructuring the Response
    DeleteCalculatedAttributeDefinitionResponse (..),
    newDeleteCalculatedAttributeDefinitionResponse,

    -- * Response Lenses
    deleteCalculatedAttributeDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCalculatedAttributeDefinition' smart constructor.
data DeleteCalculatedAttributeDefinition = DeleteCalculatedAttributeDefinition'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCalculatedAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteCalculatedAttributeDefinition_domainName' - The unique name of the domain.
--
-- 'calculatedAttributeName', 'deleteCalculatedAttributeDefinition_calculatedAttributeName' - The unique name of the calculated attribute.
newDeleteCalculatedAttributeDefinition ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'calculatedAttributeName'
  Prelude.Text ->
  DeleteCalculatedAttributeDefinition
newDeleteCalculatedAttributeDefinition
  pDomainName_
  pCalculatedAttributeName_ =
    DeleteCalculatedAttributeDefinition'
      { domainName =
          pDomainName_,
        calculatedAttributeName =
          pCalculatedAttributeName_
      }

-- | The unique name of the domain.
deleteCalculatedAttributeDefinition_domainName :: Lens.Lens' DeleteCalculatedAttributeDefinition Prelude.Text
deleteCalculatedAttributeDefinition_domainName = Lens.lens (\DeleteCalculatedAttributeDefinition' {domainName} -> domainName) (\s@DeleteCalculatedAttributeDefinition' {} a -> s {domainName = a} :: DeleteCalculatedAttributeDefinition)

-- | The unique name of the calculated attribute.
deleteCalculatedAttributeDefinition_calculatedAttributeName :: Lens.Lens' DeleteCalculatedAttributeDefinition Prelude.Text
deleteCalculatedAttributeDefinition_calculatedAttributeName = Lens.lens (\DeleteCalculatedAttributeDefinition' {calculatedAttributeName} -> calculatedAttributeName) (\s@DeleteCalculatedAttributeDefinition' {} a -> s {calculatedAttributeName = a} :: DeleteCalculatedAttributeDefinition)

instance
  Core.AWSRequest
    DeleteCalculatedAttributeDefinition
  where
  type
    AWSResponse DeleteCalculatedAttributeDefinition =
      DeleteCalculatedAttributeDefinitionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCalculatedAttributeDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteCalculatedAttributeDefinition
  where
  hashWithSalt
    _salt
    DeleteCalculatedAttributeDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` calculatedAttributeName

instance
  Prelude.NFData
    DeleteCalculatedAttributeDefinition
  where
  rnf DeleteCalculatedAttributeDefinition' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf calculatedAttributeName

instance
  Data.ToHeaders
    DeleteCalculatedAttributeDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DeleteCalculatedAttributeDefinition
  where
  toPath DeleteCalculatedAttributeDefinition' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/calculated-attributes/",
        Data.toBS calculatedAttributeName
      ]

instance
  Data.ToQuery
    DeleteCalculatedAttributeDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCalculatedAttributeDefinitionResponse' smart constructor.
data DeleteCalculatedAttributeDefinitionResponse = DeleteCalculatedAttributeDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCalculatedAttributeDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCalculatedAttributeDefinitionResponse_httpStatus' - The response's http status code.
newDeleteCalculatedAttributeDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCalculatedAttributeDefinitionResponse
newDeleteCalculatedAttributeDefinitionResponse
  pHttpStatus_ =
    DeleteCalculatedAttributeDefinitionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteCalculatedAttributeDefinitionResponse_httpStatus :: Lens.Lens' DeleteCalculatedAttributeDefinitionResponse Prelude.Int
deleteCalculatedAttributeDefinitionResponse_httpStatus = Lens.lens (\DeleteCalculatedAttributeDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteCalculatedAttributeDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteCalculatedAttributeDefinitionResponse)

instance
  Prelude.NFData
    DeleteCalculatedAttributeDefinitionResponse
  where
  rnf DeleteCalculatedAttributeDefinitionResponse' {..} =
    Prelude.rnf httpStatus
