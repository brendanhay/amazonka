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
-- Module      : Network.AWS.LexModels.DeleteSlotType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of the slot type, including the @$LATEST@ version.
-- To delete a specific version of the slot type, use the
-- DeleteSlotTypeVersion operation.
--
-- You can delete a version of a slot type only if it is not referenced. To
-- delete a slot type that is referred to in one or more intents, you must
-- remove those references first.
--
-- If you get the @ResourceInUseException@ exception, the exception
-- provides an example reference that shows the intent where the slot type
-- is referenced. To remove the reference to the slot type, either update
-- the intent or delete it. If you get the same exception when you attempt
-- to delete the slot type again, repeat until the slot type has no
-- references and the @DeleteSlotType@ call is successful.
--
-- This operation requires permission for the @lex:DeleteSlotType@ action.
module Network.AWS.LexModels.DeleteSlotType
  ( -- * Creating a Request
    DeleteSlotType (..),
    newDeleteSlotType,

    -- * Request Lenses
    deleteSlotType_name,

    -- * Destructuring the Response
    DeleteSlotTypeResponse (..),
    newDeleteSlotTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSlotType' smart constructor.
data DeleteSlotType = DeleteSlotType'
  { -- | The name of the slot type. The name is case sensitive.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlotType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteSlotType_name' - The name of the slot type. The name is case sensitive.
newDeleteSlotType ::
  -- | 'name'
  Prelude.Text ->
  DeleteSlotType
newDeleteSlotType pName_ =
  DeleteSlotType' {name = pName_}

-- | The name of the slot type. The name is case sensitive.
deleteSlotType_name :: Lens.Lens' DeleteSlotType Prelude.Text
deleteSlotType_name = Lens.lens (\DeleteSlotType' {name} -> name) (\s@DeleteSlotType' {} a -> s {name = a} :: DeleteSlotType)

instance Prelude.AWSRequest DeleteSlotType where
  type Rs DeleteSlotType = DeleteSlotTypeResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteSlotTypeResponse'

instance Prelude.Hashable DeleteSlotType

instance Prelude.NFData DeleteSlotType

instance Prelude.ToHeaders DeleteSlotType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteSlotType where
  toPath DeleteSlotType' {..} =
    Prelude.mconcat ["/slottypes/", Prelude.toBS name]

instance Prelude.ToQuery DeleteSlotType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSlotTypeResponse' smart constructor.
data DeleteSlotTypeResponse = DeleteSlotTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSlotTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSlotTypeResponse ::
  DeleteSlotTypeResponse
newDeleteSlotTypeResponse = DeleteSlotTypeResponse'

instance Prelude.NFData DeleteSlotTypeResponse
