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
-- Module      : Amazonka.Route53RecoveryReadiness.DeleteResourceSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource set.
module Amazonka.Route53RecoveryReadiness.DeleteResourceSet
  ( -- * Creating a Request
    DeleteResourceSet (..),
    newDeleteResourceSet,

    -- * Request Lenses
    deleteResourceSet_resourceSetName,

    -- * Destructuring the Response
    DeleteResourceSetResponse (..),
    newDeleteResourceSetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newDeleteResourceSet' smart constructor.
data DeleteResourceSet = DeleteResourceSet'
  { -- | Name of a resource set.
    resourceSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetName', 'deleteResourceSet_resourceSetName' - Name of a resource set.
newDeleteResourceSet ::
  -- | 'resourceSetName'
  Prelude.Text ->
  DeleteResourceSet
newDeleteResourceSet pResourceSetName_ =
  DeleteResourceSet'
    { resourceSetName =
        pResourceSetName_
    }

-- | Name of a resource set.
deleteResourceSet_resourceSetName :: Lens.Lens' DeleteResourceSet Prelude.Text
deleteResourceSet_resourceSetName = Lens.lens (\DeleteResourceSet' {resourceSetName} -> resourceSetName) (\s@DeleteResourceSet' {} a -> s {resourceSetName = a} :: DeleteResourceSet)

instance Core.AWSRequest DeleteResourceSet where
  type
    AWSResponse DeleteResourceSet =
      DeleteResourceSetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteResourceSetResponse'

instance Prelude.Hashable DeleteResourceSet where
  hashWithSalt _salt DeleteResourceSet' {..} =
    _salt `Prelude.hashWithSalt` resourceSetName

instance Prelude.NFData DeleteResourceSet where
  rnf DeleteResourceSet' {..} =
    Prelude.rnf resourceSetName

instance Data.ToHeaders DeleteResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteResourceSet where
  toPath DeleteResourceSet' {..} =
    Prelude.mconcat
      ["/resourcesets/", Data.toBS resourceSetName]

instance Data.ToQuery DeleteResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceSetResponse' smart constructor.
data DeleteResourceSetResponse = DeleteResourceSetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourceSetResponse ::
  DeleteResourceSetResponse
newDeleteResourceSetResponse =
  DeleteResourceSetResponse'

instance Prelude.NFData DeleteResourceSetResponse where
  rnf _ = ()
