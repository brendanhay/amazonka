{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a dimension that you can use to limit the scope of a metric used in a security profile for AWS IoT Device Defender. For example, using a @TOPIC_FILTER@ dimension, you can narrow down the scope of the metric only to MQTT topics whose name match the pattern specified in the dimension.
module Network.AWS.IoT.CreateDimension
  ( -- * Creating a request
    CreateDimension (..),
    mkCreateDimension,

    -- ** Request lenses
    cdTags,
    cdName,
    cdType,
    cdStringValues,
    cdClientRequestToken,

    -- * Destructuring the response
    CreateDimensionResponse (..),
    mkCreateDimensionResponse,

    -- ** Response lenses
    cdrsArn,
    cdrsName,
    cdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDimension' smart constructor.
data CreateDimension = CreateDimension'
  { tags :: Lude.Maybe [Tag],
    name :: Lude.Text,
    type' :: DimensionType,
    stringValues :: Lude.NonEmpty Lude.Text,
    clientRequestToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDimension' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - Each dimension must have a unique client request token. If you try to create a new dimension with the same token as a dimension that already exists, an exception occurs. If you omit this value, AWS SDKs will automatically generate a unique client request.
-- * 'name' - A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
-- * 'stringValues' - Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
-- * 'tags' - Metadata that can be used to manage the dimension.
-- * 'type'' - Specifies the type of dimension. Supported types: @TOPIC_FILTER.@
mkCreateDimension ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  DimensionType ->
  -- | 'stringValues'
  Lude.NonEmpty Lude.Text ->
  -- | 'clientRequestToken'
  Lude.Text ->
  CreateDimension
mkCreateDimension pName_ pType_ pStringValues_ pClientRequestToken_ =
  CreateDimension'
    { tags = Lude.Nothing,
      name = pName_,
      type' = pType_,
      stringValues = pStringValues_,
      clientRequestToken = pClientRequestToken_
    }

-- | Metadata that can be used to manage the dimension.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTags :: Lens.Lens' CreateDimension (Lude.Maybe [Tag])
cdTags = Lens.lens (tags :: CreateDimension -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDimension)
{-# DEPRECATED cdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdName :: Lens.Lens' CreateDimension Lude.Text
cdName = Lens.lens (name :: CreateDimension -> Lude.Text) (\s a -> s {name = a} :: CreateDimension)
{-# DEPRECATED cdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the type of dimension. Supported types: @TOPIC_FILTER.@
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdType :: Lens.Lens' CreateDimension DimensionType
cdType = Lens.lens (type' :: CreateDimension -> DimensionType) (\s a -> s {type' = a} :: CreateDimension)
{-# DEPRECATED cdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
--
-- /Note:/ Consider using 'stringValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStringValues :: Lens.Lens' CreateDimension (Lude.NonEmpty Lude.Text)
cdStringValues = Lens.lens (stringValues :: CreateDimension -> Lude.NonEmpty Lude.Text) (\s a -> s {stringValues = a} :: CreateDimension)
{-# DEPRECATED cdStringValues "Use generic-lens or generic-optics with 'stringValues' instead." #-}

-- | Each dimension must have a unique client request token. If you try to create a new dimension with the same token as a dimension that already exists, an exception occurs. If you omit this value, AWS SDKs will automatically generate a unique client request.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdClientRequestToken :: Lens.Lens' CreateDimension Lude.Text
cdClientRequestToken = Lens.lens (clientRequestToken :: CreateDimension -> Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateDimension)
{-# DEPRECATED cdClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

instance Lude.AWSRequest CreateDimension where
  type Rs CreateDimension = CreateDimensionResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDimensionResponse'
            Lude.<$> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDimension where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateDimension where
  toJSON CreateDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("type" Lude..= type'),
            Lude.Just ("stringValues" Lude..= stringValues),
            Lude.Just ("clientRequestToken" Lude..= clientRequestToken)
          ]
      )

instance Lude.ToPath CreateDimension where
  toPath CreateDimension' {..} =
    Lude.mconcat ["/dimensions/", Lude.toBS name]

instance Lude.ToQuery CreateDimension where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDimensionResponse' smart constructor.
data CreateDimensionResponse = CreateDimensionResponse'
  { arn ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateDimensionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN (Amazon resource name) of the created dimension.
-- * 'name' - A unique identifier for the dimension.
-- * 'responseStatus' - The response status code.
mkCreateDimensionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDimensionResponse
mkCreateDimensionResponse pResponseStatus_ =
  CreateDimensionResponse'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN (Amazon resource name) of the created dimension.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsArn :: Lens.Lens' CreateDimensionResponse (Lude.Maybe Lude.Text)
cdrsArn = Lens.lens (arn :: CreateDimensionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CreateDimensionResponse)
{-# DEPRECATED cdrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A unique identifier for the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsName :: Lens.Lens' CreateDimensionResponse (Lude.Maybe Lude.Text)
cdrsName = Lens.lens (name :: CreateDimensionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateDimensionResponse)
{-# DEPRECATED cdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDimensionResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDimensionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDimensionResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
