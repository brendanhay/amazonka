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
    appTags,
    appId,
    appARN,
    appName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about an application.
--
-- /See:/ 'mkApplicationResponse' smart constructor.
data ApplicationResponse = ApplicationResponse'
  { tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    id :: Lude.Text,
    arn :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the application.
-- * 'id' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'name' - The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
-- * 'tags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
mkApplicationResponse ::
  -- | 'id'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ApplicationResponse
mkApplicationResponse pId_ pARN_ pName_ =
  ApplicationResponse'
    { tags = Lude.Nothing,
      id = pId_,
      arn = pARN_,
      name = pName_
    }

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the application. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appTags :: Lens.Lens' ApplicationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
appTags = Lens.lens (tags :: ApplicationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ApplicationResponse)
{-# DEPRECATED appTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appId :: Lens.Lens' ApplicationResponse Lude.Text
appId = Lens.lens (id :: ApplicationResponse -> Lude.Text) (\s a -> s {id = a} :: ApplicationResponse)
{-# DEPRECATED appId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appARN :: Lens.Lens' ApplicationResponse Lude.Text
appARN = Lens.lens (arn :: ApplicationResponse -> Lude.Text) (\s a -> s {arn = a} :: ApplicationResponse)
{-# DEPRECATED appARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
appName :: Lens.Lens' ApplicationResponse Lude.Text
appName = Lens.lens (name :: ApplicationResponse -> Lude.Text) (\s a -> s {name = a} :: ApplicationResponse)
{-# DEPRECATED appName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ApplicationResponse where
  parseJSON =
    Lude.withObject
      "ApplicationResponse"
      ( \x ->
          ApplicationResponse'
            Lude.<$> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "Arn")
            Lude.<*> (x Lude..: "Name")
      )
