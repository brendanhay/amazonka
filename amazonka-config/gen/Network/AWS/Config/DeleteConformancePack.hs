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
-- Module      : Network.AWS.Config.DeleteConformancePack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified conformance pack and all the AWS Config rules,
-- remediation actions, and all evaluation results within that conformance
-- pack.
--
-- AWS Config sets the conformance pack to @DELETE_IN_PROGRESS@ until the
-- deletion is complete. You cannot update a conformance pack while it is
-- in this state.
module Network.AWS.Config.DeleteConformancePack
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

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConformancePack' smart constructor.
data DeleteConformancePack = DeleteConformancePack'
  { -- | Name of the conformance pack you want to delete.
    conformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteConformancePack where
  type
    Rs DeleteConformancePack =
      DeleteConformancePackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteConformancePackResponse'

instance Prelude.Hashable DeleteConformancePack

instance Prelude.NFData DeleteConformancePack

instance Prelude.ToHeaders DeleteConformancePack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeleteConformancePack" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteConformancePack where
  toJSON DeleteConformancePack' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConformancePackName"
                  Prelude..= conformancePackName
              )
          ]
      )

instance Prelude.ToPath DeleteConformancePack where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteConformancePack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConformancePackResponse' smart constructor.
data DeleteConformancePackResponse = DeleteConformancePackResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteConformancePackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConformancePackResponse ::
  DeleteConformancePackResponse
newDeleteConformancePackResponse =
  DeleteConformancePackResponse'

instance Prelude.NFData DeleteConformancePackResponse
