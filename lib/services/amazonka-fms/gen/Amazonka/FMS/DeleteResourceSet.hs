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
-- Module      : Amazonka.FMS.DeleteResourceSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified ResourceSet.
module Amazonka.FMS.DeleteResourceSet
  ( -- * Creating a Request
    DeleteResourceSet (..),
    newDeleteResourceSet,

    -- * Request Lenses
    deleteResourceSet_identifier,

    -- * Destructuring the Response
    DeleteResourceSetResponse (..),
    newDeleteResourceSetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourceSet' smart constructor.
data DeleteResourceSet = DeleteResourceSet'
  { -- | A unique identifier for the resource set, used in a TODO to refer to the
    -- resource set.
    identifier :: Prelude.Text
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
-- 'identifier', 'deleteResourceSet_identifier' - A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
newDeleteResourceSet ::
  -- | 'identifier'
  Prelude.Text ->
  DeleteResourceSet
newDeleteResourceSet pIdentifier_ =
  DeleteResourceSet' {identifier = pIdentifier_}

-- | A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
deleteResourceSet_identifier :: Lens.Lens' DeleteResourceSet Prelude.Text
deleteResourceSet_identifier = Lens.lens (\DeleteResourceSet' {identifier} -> identifier) (\s@DeleteResourceSet' {} a -> s {identifier = a} :: DeleteResourceSet)

instance Core.AWSRequest DeleteResourceSet where
  type
    AWSResponse DeleteResourceSet =
      DeleteResourceSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteResourceSetResponse'

instance Prelude.Hashable DeleteResourceSet where
  hashWithSalt _salt DeleteResourceSet' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeleteResourceSet where
  rnf DeleteResourceSet' {..} = Prelude.rnf identifier

instance Data.ToHeaders DeleteResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.DeleteResourceSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourceSet where
  toJSON DeleteResourceSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )

instance Data.ToPath DeleteResourceSet where
  toPath = Prelude.const "/"

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
