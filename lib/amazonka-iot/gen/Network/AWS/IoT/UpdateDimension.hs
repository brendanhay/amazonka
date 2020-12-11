{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for a dimension. You cannot change the type of a dimension after it is created (you can delete it and re-create it).
module Network.AWS.IoT.UpdateDimension
  ( -- * Creating a request
    UpdateDimension (..),
    mkUpdateDimension,

    -- ** Request lenses
    udName,
    udStringValues,

    -- * Destructuring the response
    UpdateDimensionResponse (..),
    mkUpdateDimensionResponse,

    -- ** Response lenses
    udrsLastModifiedDate,
    udrsArn,
    udrsStringValues,
    udrsName,
    udrsCreationDate,
    udrsType,
    udrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDimension' smart constructor.
data UpdateDimension = UpdateDimension'
  { name :: Lude.Text,
    stringValues :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDimension' with the minimum fields required to make a request.
--
-- * 'name' - A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
-- * 'stringValues' - Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
mkUpdateDimension ::
  -- | 'name'
  Lude.Text ->
  -- | 'stringValues'
  Lude.NonEmpty Lude.Text ->
  UpdateDimension
mkUpdateDimension pName_ pStringValues_ =
  UpdateDimension' {name = pName_, stringValues = pStringValues_}

-- | A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UpdateDimension Lude.Text
udName = Lens.lens (name :: UpdateDimension -> Lude.Text) (\s a -> s {name = a} :: UpdateDimension)
{-# DEPRECATED udName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
--
-- /Note:/ Consider using 'stringValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udStringValues :: Lens.Lens' UpdateDimension (Lude.NonEmpty Lude.Text)
udStringValues = Lens.lens (stringValues :: UpdateDimension -> Lude.NonEmpty Lude.Text) (\s a -> s {stringValues = a} :: UpdateDimension)
{-# DEPRECATED udStringValues "Use generic-lens or generic-optics with 'stringValues' instead." #-}

instance Lude.AWSRequest UpdateDimension where
  type Rs UpdateDimension = UpdateDimensionResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDimensionResponse'
            Lude.<$> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "stringValues")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDimension where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateDimension where
  toJSON UpdateDimension' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("stringValues" Lude..= stringValues)])

instance Lude.ToPath UpdateDimension where
  toPath UpdateDimension' {..} =
    Lude.mconcat ["/dimensions/", Lude.toBS name]

instance Lude.ToQuery UpdateDimension where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDimensionResponse' smart constructor.
data UpdateDimensionResponse = UpdateDimensionResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    stringValues ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    name :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    type' :: Lude.Maybe DimensionType,
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

-- | Creates a value of 'UpdateDimensionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN (Amazon resource name) of the created dimension.
-- * 'creationDate' - The date and time, in milliseconds since epoch, when the dimension was initially created.
-- * 'lastModifiedDate' - The date and time, in milliseconds since epoch, when the dimension was most recently updated.
-- * 'name' - A unique identifier for the dimension.
-- * 'responseStatus' - The response status code.
-- * 'stringValues' - The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
-- * 'type'' - The type of the dimension.
mkUpdateDimensionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDimensionResponse
mkUpdateDimensionResponse pResponseStatus_ =
  UpdateDimensionResponse'
    { lastModifiedDate = Lude.Nothing,
      arn = Lude.Nothing,
      stringValues = Lude.Nothing,
      name = Lude.Nothing,
      creationDate = Lude.Nothing,
      type' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time, in milliseconds since epoch, when the dimension was most recently updated.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsLastModifiedDate :: Lens.Lens' UpdateDimensionResponse (Lude.Maybe Lude.Timestamp)
udrsLastModifiedDate = Lens.lens (lastModifiedDate :: UpdateDimensionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: UpdateDimensionResponse)
{-# DEPRECATED udrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ARN (Amazon resource name) of the created dimension.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsArn :: Lens.Lens' UpdateDimensionResponse (Lude.Maybe Lude.Text)
udrsArn = Lens.lens (arn :: UpdateDimensionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UpdateDimensionResponse)
{-# DEPRECATED udrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
--
-- /Note:/ Consider using 'stringValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsStringValues :: Lens.Lens' UpdateDimensionResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
udrsStringValues = Lens.lens (stringValues :: UpdateDimensionResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {stringValues = a} :: UpdateDimensionResponse)
{-# DEPRECATED udrsStringValues "Use generic-lens or generic-optics with 'stringValues' instead." #-}

-- | A unique identifier for the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsName :: Lens.Lens' UpdateDimensionResponse (Lude.Maybe Lude.Text)
udrsName = Lens.lens (name :: UpdateDimensionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateDimensionResponse)
{-# DEPRECATED udrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date and time, in milliseconds since epoch, when the dimension was initially created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsCreationDate :: Lens.Lens' UpdateDimensionResponse (Lude.Maybe Lude.Timestamp)
udrsCreationDate = Lens.lens (creationDate :: UpdateDimensionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: UpdateDimensionResponse)
{-# DEPRECATED udrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The type of the dimension.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsType :: Lens.Lens' UpdateDimensionResponse (Lude.Maybe DimensionType)
udrsType = Lens.lens (type' :: UpdateDimensionResponse -> Lude.Maybe DimensionType) (\s a -> s {type' = a} :: UpdateDimensionResponse)
{-# DEPRECATED udrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrsResponseStatus :: Lens.Lens' UpdateDimensionResponse Lude.Int
udrsResponseStatus = Lens.lens (responseStatus :: UpdateDimensionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDimensionResponse)
{-# DEPRECATED udrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
