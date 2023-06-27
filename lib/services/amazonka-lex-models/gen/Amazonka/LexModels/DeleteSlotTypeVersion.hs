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
-- Module      : Amazonka.LexModels.DeleteSlotTypeVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a slot type. To delete all versions of a
-- slot type, use the DeleteSlotType operation.
--
-- This operation requires permissions for the @lex:DeleteSlotTypeVersion@
-- action.
module Amazonka.LexModels.DeleteSlotTypeVersion
  ( -- * Creating a Request
    DeleteSlotTypeVersion (..),
    newDeleteSlotTypeVersion,

    -- * Request Lenses
    deleteSlotTypeVersion_name,
    deleteSlotTypeVersion_version,

    -- * Destructuring the Response
    DeleteSlotTypeVersionResponse (..),
    newDeleteSlotTypeVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSlotTypeVersion' smart constructor.
data DeleteSlotTypeVersion = DeleteSlotTypeVersion'
  { -- | The name of the slot type.
    name :: Prelude.Text,
    -- | The version of the slot type to delete. You cannot delete the @$LATEST@
    -- version of the slot type. To delete the @$LATEST@ version, use the
    -- DeleteSlotType operation.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlotTypeVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteSlotTypeVersion_name' - The name of the slot type.
--
-- 'version', 'deleteSlotTypeVersion_version' - The version of the slot type to delete. You cannot delete the @$LATEST@
-- version of the slot type. To delete the @$LATEST@ version, use the
-- DeleteSlotType operation.
newDeleteSlotTypeVersion ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  DeleteSlotTypeVersion
newDeleteSlotTypeVersion pName_ pVersion_ =
  DeleteSlotTypeVersion'
    { name = pName_,
      version = pVersion_
    }

-- | The name of the slot type.
deleteSlotTypeVersion_name :: Lens.Lens' DeleteSlotTypeVersion Prelude.Text
deleteSlotTypeVersion_name = Lens.lens (\DeleteSlotTypeVersion' {name} -> name) (\s@DeleteSlotTypeVersion' {} a -> s {name = a} :: DeleteSlotTypeVersion)

-- | The version of the slot type to delete. You cannot delete the @$LATEST@
-- version of the slot type. To delete the @$LATEST@ version, use the
-- DeleteSlotType operation.
deleteSlotTypeVersion_version :: Lens.Lens' DeleteSlotTypeVersion Prelude.Text
deleteSlotTypeVersion_version = Lens.lens (\DeleteSlotTypeVersion' {version} -> version) (\s@DeleteSlotTypeVersion' {} a -> s {version = a} :: DeleteSlotTypeVersion)

instance Core.AWSRequest DeleteSlotTypeVersion where
  type
    AWSResponse DeleteSlotTypeVersion =
      DeleteSlotTypeVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteSlotTypeVersionResponse'

instance Prelude.Hashable DeleteSlotTypeVersion where
  hashWithSalt _salt DeleteSlotTypeVersion' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData DeleteSlotTypeVersion where
  rnf DeleteSlotTypeVersion' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Data.ToHeaders DeleteSlotTypeVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSlotTypeVersion where
  toPath DeleteSlotTypeVersion' {..} =
    Prelude.mconcat
      [ "/slottypes/",
        Data.toBS name,
        "/version/",
        Data.toBS version
      ]

instance Data.ToQuery DeleteSlotTypeVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSlotTypeVersionResponse' smart constructor.
data DeleteSlotTypeVersionResponse = DeleteSlotTypeVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlotTypeVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSlotTypeVersionResponse ::
  DeleteSlotTypeVersionResponse
newDeleteSlotTypeVersionResponse =
  DeleteSlotTypeVersionResponse'

instance Prelude.NFData DeleteSlotTypeVersionResponse where
  rnf _ = ()
