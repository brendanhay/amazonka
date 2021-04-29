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
-- Module      : Network.AWS.IoT.DeleteThingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing type. You cannot delete a thing type if it
-- has things associated with it. To delete a thing type, first mark it as
-- deprecated by calling DeprecateThingType, then remove any associated
-- things by calling UpdateThing to change the thing type on any associated
-- thing, and finally use DeleteThingType to delete the thing type.
module Network.AWS.IoT.DeleteThingType
  ( -- * Creating a Request
    DeleteThingType (..),
    newDeleteThingType,

    -- * Request Lenses
    deleteThingType_thingTypeName,

    -- * Destructuring the Response
    DeleteThingTypeResponse (..),
    newDeleteThingTypeResponse,

    -- * Response Lenses
    deleteThingTypeResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteThingType operation.
--
-- /See:/ 'newDeleteThingType' smart constructor.
data DeleteThingType = DeleteThingType'
  { -- | The name of the thing type.
    thingTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteThingType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypeName', 'deleteThingType_thingTypeName' - The name of the thing type.
newDeleteThingType ::
  -- | 'thingTypeName'
  Prelude.Text ->
  DeleteThingType
newDeleteThingType pThingTypeName_ =
  DeleteThingType' {thingTypeName = pThingTypeName_}

-- | The name of the thing type.
deleteThingType_thingTypeName :: Lens.Lens' DeleteThingType Prelude.Text
deleteThingType_thingTypeName = Lens.lens (\DeleteThingType' {thingTypeName} -> thingTypeName) (\s@DeleteThingType' {} a -> s {thingTypeName = a} :: DeleteThingType)

instance Prelude.AWSRequest DeleteThingType where
  type Rs DeleteThingType = DeleteThingTypeResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteThingTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteThingType

instance Prelude.NFData DeleteThingType

instance Prelude.ToHeaders DeleteThingType where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteThingType where
  toPath DeleteThingType' {..} =
    Prelude.mconcat
      ["/thing-types/", Prelude.toBS thingTypeName]

instance Prelude.ToQuery DeleteThingType where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DeleteThingType operation.
--
-- /See:/ 'newDeleteThingTypeResponse' smart constructor.
data DeleteThingTypeResponse = DeleteThingTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteThingTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteThingTypeResponse_httpStatus' - The response's http status code.
newDeleteThingTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteThingTypeResponse
newDeleteThingTypeResponse pHttpStatus_ =
  DeleteThingTypeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteThingTypeResponse_httpStatus :: Lens.Lens' DeleteThingTypeResponse Prelude.Int
deleteThingTypeResponse_httpStatus = Lens.lens (\DeleteThingTypeResponse' {httpStatus} -> httpStatus) (\s@DeleteThingTypeResponse' {} a -> s {httpStatus = a} :: DeleteThingTypeResponse)

instance Prelude.NFData DeleteThingTypeResponse
