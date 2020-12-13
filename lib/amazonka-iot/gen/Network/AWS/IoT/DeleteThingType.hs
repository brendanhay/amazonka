{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing type. You cannot delete a thing type if it has things associated with it. To delete a thing type, first mark it as deprecated by calling 'DeprecateThingType' , then remove any associated things by calling 'UpdateThing' to change the thing type on any associated thing, and finally use 'DeleteThingType' to delete the thing type.
module Network.AWS.IoT.DeleteThingType
  ( -- * Creating a request
    DeleteThingType (..),
    mkDeleteThingType,

    -- ** Request lenses
    dttThingTypeName,

    -- * Destructuring the response
    DeleteThingTypeResponse (..),
    mkDeleteThingTypeResponse,

    -- ** Response lenses
    dttrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeleteThingType operation.
--
-- /See:/ 'mkDeleteThingType' smart constructor.
newtype DeleteThingType = DeleteThingType'
  { -- | The name of the thing type.
    thingTypeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThingType' with the minimum fields required to make a request.
--
-- * 'thingTypeName' - The name of the thing type.
mkDeleteThingType ::
  -- | 'thingTypeName'
  Lude.Text ->
  DeleteThingType
mkDeleteThingType pThingTypeName_ =
  DeleteThingType' {thingTypeName = pThingTypeName_}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttThingTypeName :: Lens.Lens' DeleteThingType Lude.Text
dttThingTypeName = Lens.lens (thingTypeName :: DeleteThingType -> Lude.Text) (\s a -> s {thingTypeName = a} :: DeleteThingType)
{-# DEPRECATED dttThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

instance Lude.AWSRequest DeleteThingType where
  type Rs DeleteThingType = DeleteThingTypeResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteThingTypeResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteThingType where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteThingType where
  toPath DeleteThingType' {..} =
    Lude.mconcat ["/thing-types/", Lude.toBS thingTypeName]

instance Lude.ToQuery DeleteThingType where
  toQuery = Lude.const Lude.mempty

-- | The output for the DeleteThingType operation.
--
-- /See:/ 'mkDeleteThingTypeResponse' smart constructor.
newtype DeleteThingTypeResponse = DeleteThingTypeResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThingTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteThingTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteThingTypeResponse
mkDeleteThingTypeResponse pResponseStatus_ =
  DeleteThingTypeResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrsResponseStatus :: Lens.Lens' DeleteThingTypeResponse Lude.Int
dttrsResponseStatus = Lens.lens (responseStatus :: DeleteThingTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteThingTypeResponse)
{-# DEPRECATED dttrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
