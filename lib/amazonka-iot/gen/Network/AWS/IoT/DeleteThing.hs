{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified thing. Returns successfully with no error if the deletion is successful or you specify a thing that doesn't exist.
module Network.AWS.IoT.DeleteThing
  ( -- * Creating a request
    DeleteThing (..),
    mkDeleteThing,

    -- ** Request lenses
    dtExpectedVersion,
    dtThingName,

    -- * Destructuring the response
    DeleteThingResponse (..),
    mkDeleteThingResponse,

    -- ** Response lenses
    delersResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeleteThing operation.
--
-- /See:/ 'mkDeleteThing' smart constructor.
data DeleteThing = DeleteThing'
  { expectedVersion ::
      Lude.Maybe Lude.Integer,
    thingName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThing' with the minimum fields required to make a request.
--
-- * 'expectedVersion' - The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @DeleteThing@ request is rejected with a @VersionConflictException@ .
-- * 'thingName' - The name of the thing to delete.
mkDeleteThing ::
  -- | 'thingName'
  Lude.Text ->
  DeleteThing
mkDeleteThing pThingName_ =
  DeleteThing'
    { expectedVersion = Lude.Nothing,
      thingName = pThingName_
    }

-- | The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @DeleteThing@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtExpectedVersion :: Lens.Lens' DeleteThing (Lude.Maybe Lude.Integer)
dtExpectedVersion = Lens.lens (expectedVersion :: DeleteThing -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: DeleteThing)
{-# DEPRECATED dtExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the thing to delete.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtThingName :: Lens.Lens' DeleteThing Lude.Text
dtThingName = Lens.lens (thingName :: DeleteThing -> Lude.Text) (\s a -> s {thingName = a} :: DeleteThing)
{-# DEPRECATED dtThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest DeleteThing where
  type Rs DeleteThing = DeleteThingResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteThingResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteThing where
  toPath DeleteThing' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName]

instance Lude.ToQuery DeleteThing where
  toQuery DeleteThing' {..} =
    Lude.mconcat ["expectedVersion" Lude.=: expectedVersion]

-- | The output of the DeleteThing operation.
--
-- /See:/ 'mkDeleteThingResponse' smart constructor.
newtype DeleteThingResponse = DeleteThingResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteThingResponse
mkDeleteThingResponse pResponseStatus_ =
  DeleteThingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delersResponseStatus :: Lens.Lens' DeleteThingResponse Lude.Int
delersResponseStatus = Lens.lens (responseStatus :: DeleteThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteThingResponse)
{-# DEPRECATED delersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
