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
-- Module      : Amazonka.Config.DeleteConformancePack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified conformance pack and all the Config rules,
-- remediation actions, and all evaluation results within that conformance
-- pack.
--
-- Config sets the conformance pack to @DELETE_IN_PROGRESS@ until the
-- deletion is complete. You cannot update a conformance pack while it is
-- in this state.
module Amazonka.Config.DeleteConformancePack
  ( -- * Creating a Request
    DeleteConformancePack (..),
    newDeleteConformancePack,

    -- * Request Lenses
    deleteConformancePack_conformancePackName,

    -- * Destructuring the Response
    DeleteConformancePackResponse (..),
    newDeleteConformancePackResponse,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConformancePack' smart constructor.
data DeleteConformancePack = DeleteConformancePack'
  { -- | Name of the conformance pack you want to delete.
    conformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackName', 'deleteConformancePack_conformancePackName' - Name of the conformance pack you want to delete.
newDeleteConformancePack ::
  -- | 'conformancePackName'
  Prelude.Text ->
  DeleteConformancePack
newDeleteConformancePack pConformancePackName_ =
  DeleteConformancePack'
    { conformancePackName =
        pConformancePackName_
    }

-- | Name of the conformance pack you want to delete.
deleteConformancePack_conformancePackName :: Lens.Lens' DeleteConformancePack Prelude.Text
deleteConformancePack_conformancePackName = Lens.lens (\DeleteConformancePack' {conformancePackName} -> conformancePackName) (\s@DeleteConformancePack' {} a -> s {conformancePackName = a} :: DeleteConformancePack)

instance Core.AWSRequest DeleteConformancePack where
  type
    AWSResponse DeleteConformancePack =
      DeleteConformancePackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteConformancePackResponse'

instance Prelude.Hashable DeleteConformancePack where
  hashWithSalt _salt DeleteConformancePack' {..} =
    _salt `Prelude.hashWithSalt` conformancePackName

instance Prelude.NFData DeleteConformancePack where
  rnf DeleteConformancePack' {..} =
    Prelude.rnf conformancePackName

instance Data.ToHeaders DeleteConformancePack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteConformancePack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConformancePack where
  toJSON DeleteConformancePack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConformancePackName" Data..= conformancePackName)
          ]
      )

instance Data.ToPath DeleteConformancePack where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConformancePack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConformancePackResponse' smart constructor.
data DeleteConformancePackResponse = DeleteConformancePackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConformancePackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConformancePackResponse ::
  DeleteConformancePackResponse
newDeleteConformancePackResponse =
  DeleteConformancePackResponse'

instance Prelude.NFData DeleteConformancePackResponse where
  rnf _ = ()
