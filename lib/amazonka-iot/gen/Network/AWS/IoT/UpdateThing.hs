{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data for a thing.
module Network.AWS.IoT.UpdateThing
  ( -- * Creating a request
    UpdateThing (..),
    mkUpdateThing,

    -- ** Request lenses
    utRemoveThingType,
    utThingTypeName,
    utExpectedVersion,
    utAttributePayload,
    utThingName,

    -- * Destructuring the response
    UpdateThingResponse (..),
    mkUpdateThingResponse,

    -- ** Response lenses
    utrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the UpdateThing operation.
--
-- /See:/ 'mkUpdateThing' smart constructor.
data UpdateThing = UpdateThing'
  { removeThingType ::
      Lude.Maybe Lude.Bool,
    thingTypeName :: Lude.Maybe Lude.Text,
    expectedVersion :: Lude.Maybe Lude.Integer,
    attributePayload :: Lude.Maybe AttributePayload,
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

-- | Creates a value of 'UpdateThing' with the minimum fields required to make a request.
--
-- * 'attributePayload' - A list of thing attributes, a JSON string containing name-value pairs. For example:
--
-- @{\"attributes\":{\"name1\":\"value2\"}}@
-- This data is used to add new attributes or update existing attributes.
-- * 'expectedVersion' - The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @UpdateThing@ request is rejected with a @VersionConflictException@ .
-- * 'removeThingType' - Remove a thing type association. If __true__ , the association is removed.
-- * 'thingName' - The name of the thing to update.
--
-- You can't change a thing's name. To change a thing's name, you must create a new thing, give it the new name, and then delete the old thing.
-- * 'thingTypeName' - The name of the thing type.
mkUpdateThing ::
  -- | 'thingName'
  Lude.Text ->
  UpdateThing
mkUpdateThing pThingName_ =
  UpdateThing'
    { removeThingType = Lude.Nothing,
      thingTypeName = Lude.Nothing,
      expectedVersion = Lude.Nothing,
      attributePayload = Lude.Nothing,
      thingName = pThingName_
    }

-- | Remove a thing type association. If __true__ , the association is removed.
--
-- /Note:/ Consider using 'removeThingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utRemoveThingType :: Lens.Lens' UpdateThing (Lude.Maybe Lude.Bool)
utRemoveThingType = Lens.lens (removeThingType :: UpdateThing -> Lude.Maybe Lude.Bool) (\s a -> s {removeThingType = a} :: UpdateThing)
{-# DEPRECATED utRemoveThingType "Use generic-lens or generic-optics with 'removeThingType' instead." #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utThingTypeName :: Lens.Lens' UpdateThing (Lude.Maybe Lude.Text)
utThingTypeName = Lens.lens (thingTypeName :: UpdateThing -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: UpdateThing)
{-# DEPRECATED utThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The expected version of the thing record in the registry. If the version of the record in the registry does not match the expected version specified in the request, the @UpdateThing@ request is rejected with a @VersionConflictException@ .
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utExpectedVersion :: Lens.Lens' UpdateThing (Lude.Maybe Lude.Integer)
utExpectedVersion = Lens.lens (expectedVersion :: UpdateThing -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: UpdateThing)
{-# DEPRECATED utExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | A list of thing attributes, a JSON string containing name-value pairs. For example:
--
-- @{\"attributes\":{\"name1\":\"value2\"}}@
-- This data is used to add new attributes or update existing attributes.
--
-- /Note:/ Consider using 'attributePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utAttributePayload :: Lens.Lens' UpdateThing (Lude.Maybe AttributePayload)
utAttributePayload = Lens.lens (attributePayload :: UpdateThing -> Lude.Maybe AttributePayload) (\s a -> s {attributePayload = a} :: UpdateThing)
{-# DEPRECATED utAttributePayload "Use generic-lens or generic-optics with 'attributePayload' instead." #-}

-- | The name of the thing to update.
--
-- You can't change a thing's name. To change a thing's name, you must create a new thing, give it the new name, and then delete the old thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utThingName :: Lens.Lens' UpdateThing Lude.Text
utThingName = Lens.lens (thingName :: UpdateThing -> Lude.Text) (\s a -> s {thingName = a} :: UpdateThing)
{-# DEPRECATED utThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest UpdateThing where
  type Rs UpdateThing = UpdateThingResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateThingResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateThing where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateThing where
  toJSON UpdateThing' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("removeThingType" Lude..=) Lude.<$> removeThingType,
            ("thingTypeName" Lude..=) Lude.<$> thingTypeName,
            ("expectedVersion" Lude..=) Lude.<$> expectedVersion,
            ("attributePayload" Lude..=) Lude.<$> attributePayload
          ]
      )

instance Lude.ToPath UpdateThing where
  toPath UpdateThing' {..} =
    Lude.mconcat ["/things/", Lude.toBS thingName]

instance Lude.ToQuery UpdateThing where
  toQuery = Lude.const Lude.mempty

-- | The output from the UpdateThing operation.
--
-- /See:/ 'mkUpdateThingResponse' smart constructor.
newtype UpdateThingResponse = UpdateThingResponse'
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

-- | Creates a value of 'UpdateThingResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateThingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateThingResponse
mkUpdateThingResponse pResponseStatus_ =
  UpdateThingResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrsResponseStatus :: Lens.Lens' UpdateThingResponse Lude.Int
utrsResponseStatus = Lens.lens (responseStatus :: UpdateThingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateThingResponse)
{-# DEPRECATED utrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
