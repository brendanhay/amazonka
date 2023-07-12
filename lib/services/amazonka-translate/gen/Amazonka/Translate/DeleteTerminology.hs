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
-- Module      : Amazonka.Translate.DeleteTerminology
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A synchronous action that deletes a custom terminology.
module Amazonka.Translate.DeleteTerminology
  ( -- * Creating a Request
    DeleteTerminology (..),
    newDeleteTerminology,

    -- * Request Lenses
    deleteTerminology_name,

    -- * Destructuring the Response
    DeleteTerminologyResponse (..),
    newDeleteTerminologyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newDeleteTerminology' smart constructor.
data DeleteTerminology = DeleteTerminology'
  { -- | The name of the custom terminology being deleted.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTerminology' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteTerminology_name' - The name of the custom terminology being deleted.
newDeleteTerminology ::
  -- | 'name'
  Prelude.Text ->
  DeleteTerminology
newDeleteTerminology pName_ =
  DeleteTerminology' {name = pName_}

-- | The name of the custom terminology being deleted.
deleteTerminology_name :: Lens.Lens' DeleteTerminology Prelude.Text
deleteTerminology_name = Lens.lens (\DeleteTerminology' {name} -> name) (\s@DeleteTerminology' {} a -> s {name = a} :: DeleteTerminology)

instance Core.AWSRequest DeleteTerminology where
  type
    AWSResponse DeleteTerminology =
      DeleteTerminologyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteTerminologyResponse'

instance Prelude.Hashable DeleteTerminology where
  hashWithSalt _salt DeleteTerminology' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteTerminology where
  rnf DeleteTerminology' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteTerminology where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.DeleteTerminology" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTerminology where
  toJSON DeleteTerminology' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteTerminology where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTerminology where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTerminologyResponse' smart constructor.
data DeleteTerminologyResponse = DeleteTerminologyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTerminologyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteTerminologyResponse ::
  DeleteTerminologyResponse
newDeleteTerminologyResponse =
  DeleteTerminologyResponse'

instance Prelude.NFData DeleteTerminologyResponse where
  rnf _ = ()
