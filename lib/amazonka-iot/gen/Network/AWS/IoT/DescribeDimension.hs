{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about a dimension that is defined in your AWS account.
module Network.AWS.IoT.DescribeDimension
  ( -- * Creating a request
    DescribeDimension (..),
    mkDescribeDimension,

    -- ** Request lenses
    ddName,

    -- * Destructuring the response
    DescribeDimensionResponse (..),
    mkDescribeDimensionResponse,

    -- ** Response lenses
    dddrsLastModifiedDate,
    dddrsArn,
    dddrsStringValues,
    dddrsName,
    dddrsCreationDate,
    dddrsType,
    dddrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDimension' smart constructor.
newtype DescribeDimension = DescribeDimension' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDimension' with the minimum fields required to make a request.
--
-- * 'name' - The unique identifier for the dimension.
mkDescribeDimension ::
  -- | 'name'
  Lude.Text ->
  DescribeDimension
mkDescribeDimension pName_ = DescribeDimension' {name = pName_}

-- | The unique identifier for the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddName :: Lens.Lens' DescribeDimension Lude.Text
ddName = Lens.lens (name :: DescribeDimension -> Lude.Text) (\s a -> s {name = a} :: DescribeDimension)
{-# DEPRECATED ddName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DescribeDimension where
  type Rs DescribeDimension = DescribeDimensionResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDimensionResponse'
            Lude.<$> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "stringValues")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "type")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDimension where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDimension where
  toPath DescribeDimension' {..} =
    Lude.mconcat ["/dimensions/", Lude.toBS name]

instance Lude.ToQuery DescribeDimension where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDimensionResponse' smart constructor.
data DescribeDimensionResponse = DescribeDimensionResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    stringValues ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    name :: Lude.Maybe Lude.Text,
    creationDate ::
      Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'DescribeDimensionResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN (Amazon resource name) for the dimension.
-- * 'creationDate' - The date the dimension was created.
-- * 'lastModifiedDate' - The date the dimension was last modified.
-- * 'name' - The unique identifier for the dimension.
-- * 'responseStatus' - The response status code.
-- * 'stringValues' - The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
-- * 'type'' - The type of the dimension.
mkDescribeDimensionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDimensionResponse
mkDescribeDimensionResponse pResponseStatus_ =
  DescribeDimensionResponse'
    { lastModifiedDate = Lude.Nothing,
      arn = Lude.Nothing,
      stringValues = Lude.Nothing,
      name = Lude.Nothing,
      creationDate = Lude.Nothing,
      type' = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date the dimension was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsLastModifiedDate :: Lens.Lens' DescribeDimensionResponse (Lude.Maybe Lude.Timestamp)
dddrsLastModifiedDate = Lens.lens (lastModifiedDate :: DescribeDimensionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: DescribeDimensionResponse)
{-# DEPRECATED dddrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ARN (Amazon resource name) for the dimension.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsArn :: Lens.Lens' DescribeDimensionResponse (Lude.Maybe Lude.Text)
dddrsArn = Lens.lens (arn :: DescribeDimensionResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeDimensionResponse)
{-# DEPRECATED dddrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
--
-- /Note:/ Consider using 'stringValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsStringValues :: Lens.Lens' DescribeDimensionResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
dddrsStringValues = Lens.lens (stringValues :: DescribeDimensionResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {stringValues = a} :: DescribeDimensionResponse)
{-# DEPRECATED dddrsStringValues "Use generic-lens or generic-optics with 'stringValues' instead." #-}

-- | The unique identifier for the dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsName :: Lens.Lens' DescribeDimensionResponse (Lude.Maybe Lude.Text)
dddrsName = Lens.lens (name :: DescribeDimensionResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DescribeDimensionResponse)
{-# DEPRECATED dddrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date the dimension was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsCreationDate :: Lens.Lens' DescribeDimensionResponse (Lude.Maybe Lude.Timestamp)
dddrsCreationDate = Lens.lens (creationDate :: DescribeDimensionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeDimensionResponse)
{-# DEPRECATED dddrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The type of the dimension.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsType :: Lens.Lens' DescribeDimensionResponse (Lude.Maybe DimensionType)
dddrsType = Lens.lens (type' :: DescribeDimensionResponse -> Lude.Maybe DimensionType) (\s a -> s {type' = a} :: DescribeDimensionResponse)
{-# DEPRECATED dddrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dddrsResponseStatus :: Lens.Lens' DescribeDimensionResponse Lude.Int
dddrsResponseStatus = Lens.lens (responseStatus :: DescribeDimensionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDimensionResponse)
{-# DEPRECATED dddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
