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
-- Module      : Network.AWS.LexModels.DeleteSlotTypeVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.LexModels.DeleteSlotTypeVersion
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSlotTypeVersion' smart constructor.
data DeleteSlotTypeVersion = DeleteSlotTypeVersion'
  { -- | The name of the slot type.
    name :: Prelude.Text,
    -- | The version of the slot type to delete. You cannot delete the @$LATEST@
    -- version of the slot type. To delete the @$LATEST@ version, use the
    -- DeleteSlotType operation.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteSlotTypeVersion where
  type
    Rs DeleteSlotTypeVersion =
      DeleteSlotTypeVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteSlotTypeVersionResponse'

instance Prelude.Hashable DeleteSlotTypeVersion

instance Prelude.NFData DeleteSlotTypeVersion

instance Prelude.ToHeaders DeleteSlotTypeVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteSlotTypeVersion where
  toPath DeleteSlotTypeVersion' {..} =
    Prelude.mconcat
      [ "/slottypes/",
        Prelude.toBS name,
        "/version/",
        Prelude.toBS version
      ]

instance Prelude.ToQuery DeleteSlotTypeVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSlotTypeVersionResponse' smart constructor.
data DeleteSlotTypeVersionResponse = DeleteSlotTypeVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlotTypeVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSlotTypeVersionResponse ::
  DeleteSlotTypeVersionResponse
newDeleteSlotTypeVersionResponse =
  DeleteSlotTypeVersionResponse'

instance Prelude.NFData DeleteSlotTypeVersionResponse
