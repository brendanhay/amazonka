{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a given object's attributes.
module Network.AWS.CloudDirectory.UpdateObjectAttributes
  ( -- * Creating a request
    UpdateObjectAttributes (..),
    mkUpdateObjectAttributes,

    -- ** Request lenses
    uoaDirectoryARN,
    uoaAttributeUpdates,
    uoaObjectReference,

    -- * Destructuring the response
    UpdateObjectAttributesResponse (..),
    mkUpdateObjectAttributesResponse,

    -- ** Response lenses
    uoarsObjectIdentifier,
    uoarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateObjectAttributes' smart constructor.
data UpdateObjectAttributes = UpdateObjectAttributes'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | The attributes update structure.
    attributeUpdates :: [ObjectAttributeUpdate],
    -- | The reference that identifies the object.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateObjectAttributes' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
-- * 'attributeUpdates' - The attributes update structure.
-- * 'objectReference' - The reference that identifies the object.
mkUpdateObjectAttributes ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  UpdateObjectAttributes
mkUpdateObjectAttributes pDirectoryARN_ pObjectReference_ =
  UpdateObjectAttributes'
    { directoryARN = pDirectoryARN_,
      attributeUpdates = Lude.mempty,
      objectReference = pObjectReference_
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoaDirectoryARN :: Lens.Lens' UpdateObjectAttributes Lude.Text
uoaDirectoryARN = Lens.lens (directoryARN :: UpdateObjectAttributes -> Lude.Text) (\s a -> s {directoryARN = a} :: UpdateObjectAttributes)
{-# DEPRECATED uoaDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoaAttributeUpdates :: Lens.Lens' UpdateObjectAttributes [ObjectAttributeUpdate]
uoaAttributeUpdates = Lens.lens (attributeUpdates :: UpdateObjectAttributes -> [ObjectAttributeUpdate]) (\s a -> s {attributeUpdates = a} :: UpdateObjectAttributes)
{-# DEPRECATED uoaAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

-- | The reference that identifies the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoaObjectReference :: Lens.Lens' UpdateObjectAttributes ObjectReference
uoaObjectReference = Lens.lens (objectReference :: UpdateObjectAttributes -> ObjectReference) (\s a -> s {objectReference = a} :: UpdateObjectAttributes)
{-# DEPRECATED uoaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest UpdateObjectAttributes where
  type Rs UpdateObjectAttributes = UpdateObjectAttributesResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateObjectAttributesResponse'
            Lude.<$> (x Lude..?> "ObjectIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateObjectAttributes where
  toHeaders UpdateObjectAttributes' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON UpdateObjectAttributes where
  toJSON UpdateObjectAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AttributeUpdates" Lude..= attributeUpdates),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath UpdateObjectAttributes where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/update"

instance Lude.ToQuery UpdateObjectAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateObjectAttributesResponse' smart constructor.
data UpdateObjectAttributesResponse = UpdateObjectAttributesResponse'
  { -- | The @ObjectIdentifier@ of the updated object.
    objectIdentifier :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateObjectAttributesResponse' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - The @ObjectIdentifier@ of the updated object.
-- * 'responseStatus' - The response status code.
mkUpdateObjectAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateObjectAttributesResponse
mkUpdateObjectAttributesResponse pResponseStatus_ =
  UpdateObjectAttributesResponse'
    { objectIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ObjectIdentifier@ of the updated object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoarsObjectIdentifier :: Lens.Lens' UpdateObjectAttributesResponse (Lude.Maybe Lude.Text)
uoarsObjectIdentifier = Lens.lens (objectIdentifier :: UpdateObjectAttributesResponse -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: UpdateObjectAttributesResponse)
{-# DEPRECATED uoarsObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uoarsResponseStatus :: Lens.Lens' UpdateObjectAttributesResponse Lude.Int
uoarsResponseStatus = Lens.lens (responseStatus :: UpdateObjectAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateObjectAttributesResponse)
{-# DEPRECATED uoarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
