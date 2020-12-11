{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new thing type.
module Network.AWS.IoT.CreateThingType
  ( -- * Creating a request
    CreateThingType (..),
    mkCreateThingType,

    -- ** Request lenses
    cttThingTypeProperties,
    cttTags,
    cttThingTypeName,

    -- * Destructuring the response
    CreateThingTypeResponse (..),
    mkCreateThingTypeResponse,

    -- ** Response lenses
    cttrsThingTypeName,
    cttrsThingTypeId,
    cttrsThingTypeARN,
    cttrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the CreateThingType operation.
--
-- /See:/ 'mkCreateThingType' smart constructor.
data CreateThingType = CreateThingType'
  { thingTypeProperties ::
      Lude.Maybe ThingTypeProperties,
    tags :: Lude.Maybe [Tag],
    thingTypeName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateThingType' with the minimum fields required to make a request.
--
-- * 'tags' - Metadata which can be used to manage the thing type.
-- * 'thingTypeName' - The name of the thing type.
-- * 'thingTypeProperties' - The ThingTypeProperties for the thing type to create. It contains information about the new thing type including a description, and a list of searchable thing attribute names.
mkCreateThingType ::
  -- | 'thingTypeName'
  Lude.Text ->
  CreateThingType
mkCreateThingType pThingTypeName_ =
  CreateThingType'
    { thingTypeProperties = Lude.Nothing,
      tags = Lude.Nothing,
      thingTypeName = pThingTypeName_
    }

-- | The ThingTypeProperties for the thing type to create. It contains information about the new thing type including a description, and a list of searchable thing attribute names.
--
-- /Note:/ Consider using 'thingTypeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttThingTypeProperties :: Lens.Lens' CreateThingType (Lude.Maybe ThingTypeProperties)
cttThingTypeProperties = Lens.lens (thingTypeProperties :: CreateThingType -> Lude.Maybe ThingTypeProperties) (\s a -> s {thingTypeProperties = a} :: CreateThingType)
{-# DEPRECATED cttThingTypeProperties "Use generic-lens or generic-optics with 'thingTypeProperties' instead." #-}

-- | Metadata which can be used to manage the thing type.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttTags :: Lens.Lens' CreateThingType (Lude.Maybe [Tag])
cttTags = Lens.lens (tags :: CreateThingType -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateThingType)
{-# DEPRECATED cttTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttThingTypeName :: Lens.Lens' CreateThingType Lude.Text
cttThingTypeName = Lens.lens (thingTypeName :: CreateThingType -> Lude.Text) (\s a -> s {thingTypeName = a} :: CreateThingType)
{-# DEPRECATED cttThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

instance Lude.AWSRequest CreateThingType where
  type Rs CreateThingType = CreateThingTypeResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateThingTypeResponse'
            Lude.<$> (x Lude..?> "thingTypeName")
            Lude.<*> (x Lude..?> "thingTypeId")
            Lude.<*> (x Lude..?> "thingTypeArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateThingType where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateThingType where
  toJSON CreateThingType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thingTypeProperties" Lude..=) Lude.<$> thingTypeProperties,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateThingType where
  toPath CreateThingType' {..} =
    Lude.mconcat ["/thing-types/", Lude.toBS thingTypeName]

instance Lude.ToQuery CreateThingType where
  toQuery = Lude.const Lude.mempty

-- | The output of the CreateThingType operation.
--
-- /See:/ 'mkCreateThingTypeResponse' smart constructor.
data CreateThingTypeResponse = CreateThingTypeResponse'
  { thingTypeName ::
      Lude.Maybe Lude.Text,
    thingTypeId :: Lude.Maybe Lude.Text,
    thingTypeARN :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateThingTypeResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'thingTypeARN' - The Amazon Resource Name (ARN) of the thing type.
-- * 'thingTypeId' - The thing type ID.
-- * 'thingTypeName' - The name of the thing type.
mkCreateThingTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateThingTypeResponse
mkCreateThingTypeResponse pResponseStatus_ =
  CreateThingTypeResponse'
    { thingTypeName = Lude.Nothing,
      thingTypeId = Lude.Nothing,
      thingTypeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrsThingTypeName :: Lens.Lens' CreateThingTypeResponse (Lude.Maybe Lude.Text)
cttrsThingTypeName = Lens.lens (thingTypeName :: CreateThingTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: CreateThingTypeResponse)
{-# DEPRECATED cttrsThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The thing type ID.
--
-- /Note:/ Consider using 'thingTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrsThingTypeId :: Lens.Lens' CreateThingTypeResponse (Lude.Maybe Lude.Text)
cttrsThingTypeId = Lens.lens (thingTypeId :: CreateThingTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeId = a} :: CreateThingTypeResponse)
{-# DEPRECATED cttrsThingTypeId "Use generic-lens or generic-optics with 'thingTypeId' instead." #-}

-- | The Amazon Resource Name (ARN) of the thing type.
--
-- /Note:/ Consider using 'thingTypeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrsThingTypeARN :: Lens.Lens' CreateThingTypeResponse (Lude.Maybe Lude.Text)
cttrsThingTypeARN = Lens.lens (thingTypeARN :: CreateThingTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeARN = a} :: CreateThingTypeResponse)
{-# DEPRECATED cttrsThingTypeARN "Use generic-lens or generic-optics with 'thingTypeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cttrsResponseStatus :: Lens.Lens' CreateThingTypeResponse Lude.Int
cttrsResponseStatus = Lens.lens (responseStatus :: CreateThingTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateThingTypeResponse)
{-# DEPRECATED cttrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
