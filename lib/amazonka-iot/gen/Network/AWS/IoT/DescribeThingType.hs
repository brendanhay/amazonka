{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing type.
module Network.AWS.IoT.DescribeThingType
  ( -- * Creating a request
    DescribeThingType (..),
    mkDescribeThingType,

    -- ** Request lenses
    dThingTypeName,

    -- * Destructuring the response
    DescribeThingTypeResponse (..),
    mkDescribeThingTypeResponse,

    -- ** Response lenses
    dttfrsThingTypeProperties,
    dttfrsThingTypeName,
    dttfrsThingTypeId,
    dttfrsThingTypeMetadata,
    dttfrsThingTypeARN,
    dttfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DescribeThingType operation.
--
-- /See:/ 'mkDescribeThingType' smart constructor.
newtype DescribeThingType = DescribeThingType'
  { -- | The name of the thing type.
    thingTypeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeThingType' with the minimum fields required to make a request.
--
-- * 'thingTypeName' - The name of the thing type.
mkDescribeThingType ::
  -- | 'thingTypeName'
  Lude.Text ->
  DescribeThingType
mkDescribeThingType pThingTypeName_ =
  DescribeThingType' {thingTypeName = pThingTypeName_}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dThingTypeName :: Lens.Lens' DescribeThingType Lude.Text
dThingTypeName = Lens.lens (thingTypeName :: DescribeThingType -> Lude.Text) (\s a -> s {thingTypeName = a} :: DescribeThingType)
{-# DEPRECATED dThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

instance Lude.AWSRequest DescribeThingType where
  type Rs DescribeThingType = DescribeThingTypeResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeThingTypeResponse'
            Lude.<$> (x Lude..?> "thingTypeProperties")
            Lude.<*> (x Lude..?> "thingTypeName")
            Lude.<*> (x Lude..?> "thingTypeId")
            Lude.<*> (x Lude..?> "thingTypeMetadata")
            Lude.<*> (x Lude..?> "thingTypeArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeThingType where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeThingType where
  toPath DescribeThingType' {..} =
    Lude.mconcat ["/thing-types/", Lude.toBS thingTypeName]

instance Lude.ToQuery DescribeThingType where
  toQuery = Lude.const Lude.mempty

-- | The output for the DescribeThingType operation.
--
-- /See:/ 'mkDescribeThingTypeResponse' smart constructor.
data DescribeThingTypeResponse = DescribeThingTypeResponse'
  { -- | The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
    thingTypeProperties :: Lude.Maybe ThingTypeProperties,
    -- | The name of the thing type.
    thingTypeName :: Lude.Maybe Lude.Text,
    -- | The thing type ID.
    thingTypeId :: Lude.Maybe Lude.Text,
    -- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
    thingTypeMetadata :: Lude.Maybe ThingTypeMetadata,
    -- | The thing type ARN.
    thingTypeARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeThingTypeResponse' with the minimum fields required to make a request.
--
-- * 'thingTypeProperties' - The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
-- * 'thingTypeName' - The name of the thing type.
-- * 'thingTypeId' - The thing type ID.
-- * 'thingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
-- * 'thingTypeARN' - The thing type ARN.
-- * 'responseStatus' - The response status code.
mkDescribeThingTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeThingTypeResponse
mkDescribeThingTypeResponse pResponseStatus_ =
  DescribeThingTypeResponse'
    { thingTypeProperties = Lude.Nothing,
      thingTypeName = Lude.Nothing,
      thingTypeId = Lude.Nothing,
      thingTypeMetadata = Lude.Nothing,
      thingTypeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
--
-- /Note:/ Consider using 'thingTypeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfrsThingTypeProperties :: Lens.Lens' DescribeThingTypeResponse (Lude.Maybe ThingTypeProperties)
dttfrsThingTypeProperties = Lens.lens (thingTypeProperties :: DescribeThingTypeResponse -> Lude.Maybe ThingTypeProperties) (\s a -> s {thingTypeProperties = a} :: DescribeThingTypeResponse)
{-# DEPRECATED dttfrsThingTypeProperties "Use generic-lens or generic-optics with 'thingTypeProperties' instead." #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfrsThingTypeName :: Lens.Lens' DescribeThingTypeResponse (Lude.Maybe Lude.Text)
dttfrsThingTypeName = Lens.lens (thingTypeName :: DescribeThingTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: DescribeThingTypeResponse)
{-# DEPRECATED dttfrsThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The thing type ID.
--
-- /Note:/ Consider using 'thingTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfrsThingTypeId :: Lens.Lens' DescribeThingTypeResponse (Lude.Maybe Lude.Text)
dttfrsThingTypeId = Lens.lens (thingTypeId :: DescribeThingTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeId = a} :: DescribeThingTypeResponse)
{-# DEPRECATED dttfrsThingTypeId "Use generic-lens or generic-optics with 'thingTypeId' instead." #-}

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- /Note:/ Consider using 'thingTypeMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfrsThingTypeMetadata :: Lens.Lens' DescribeThingTypeResponse (Lude.Maybe ThingTypeMetadata)
dttfrsThingTypeMetadata = Lens.lens (thingTypeMetadata :: DescribeThingTypeResponse -> Lude.Maybe ThingTypeMetadata) (\s a -> s {thingTypeMetadata = a} :: DescribeThingTypeResponse)
{-# DEPRECATED dttfrsThingTypeMetadata "Use generic-lens or generic-optics with 'thingTypeMetadata' instead." #-}

-- | The thing type ARN.
--
-- /Note:/ Consider using 'thingTypeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfrsThingTypeARN :: Lens.Lens' DescribeThingTypeResponse (Lude.Maybe Lude.Text)
dttfrsThingTypeARN = Lens.lens (thingTypeARN :: DescribeThingTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeARN = a} :: DescribeThingTypeResponse)
{-# DEPRECATED dttfrsThingTypeARN "Use generic-lens or generic-optics with 'thingTypeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttfrsResponseStatus :: Lens.Lens' DescribeThingTypeResponse Lude.Int
dttfrsResponseStatus = Lens.lens (responseStatus :: DescribeThingTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeThingTypeResponse)
{-# DEPRECATED dttfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
