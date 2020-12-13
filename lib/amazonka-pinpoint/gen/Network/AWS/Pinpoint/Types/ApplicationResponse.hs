{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ApplicationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationResponse
  ( ApplicationResponse (..),

    -- * Smart constructor
    mkApplicationResponse,

    -- * Lenses
    afARN,
    afName,
    afId,
    afTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an application.
--
-- /See:/ 'mkApplicationResponse' smart constructor.
data ApplicationResponse = ApplicationResponse'
  { -- | The Amazon Resource Name (ARN) of the application.
    arn :: Lude.Text,
    -- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
    name :: Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    id :: Lude.Text,
    -- | A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the application.
-- * 'name' - The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
-- * 'id' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'tags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
mkApplicationResponse ::
  -- | 'arn'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  ApplicationResponse
mkApplicationResponse pARN_ pName_ pId_ =
  ApplicationResponse'
    { arn = pARN_,
      name = pName_,
      id = pId_,
      tags = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afARN :: Lens.Lens' ApplicationResponse Lude.Text
afARN = Lens.lens (arn :: ApplicationResponse -> Lude.Text) (\s a -> s {arn = a} :: ApplicationResponse)
{-# DEPRECATED afARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afName :: Lens.Lens' ApplicationResponse Lude.Text
afName = Lens.lens (name :: ApplicationResponse -> Lude.Text) (\s a -> s {name = a} :: ApplicationResponse)
{-# DEPRECATED afName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afId :: Lens.Lens' ApplicationResponse Lude.Text
afId = Lens.lens (id :: ApplicationResponse -> Lude.Text) (\s a -> s {id = a} :: ApplicationResponse)
{-# DEPRECATED afId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afTags :: Lens.Lens' ApplicationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
afTags = Lens.lens (tags :: ApplicationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ApplicationResponse)
{-# DEPRECATED afTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ApplicationResponse where
  parseJSON =
    Lude.withObject
      "ApplicationResponse"
      ( \x ->
          ApplicationResponse'
            Lude.<$> (x Lude..: "Arn")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
