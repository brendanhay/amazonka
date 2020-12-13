{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CreateApplicationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CreateApplicationRequest
  ( CreateApplicationRequest (..),

    -- * Smart constructor
    mkCreateApplicationRequest,

    -- * Lenses
    carName,
    carTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the display name of an application and the tags to associate with the application.
--
-- /See:/ 'mkCreateApplicationRequest' smart constructor.
data CreateApplicationRequest = CreateApplicationRequest'
  { -- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
    name :: Lude.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to associate with the application. Each tag consists of a required tag key and an associated tag value.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplicationRequest' with the minimum fields required to make a request.
--
-- * 'name' - The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
-- * 'tags' - A string-to-string map of key-value pairs that defines the tags to associate with the application. Each tag consists of a required tag key and an associated tag value.
mkCreateApplicationRequest ::
  -- | 'name'
  Lude.Text ->
  CreateApplicationRequest
mkCreateApplicationRequest pName_ =
  CreateApplicationRequest' {name = pName_, tags = Lude.Nothing}

-- | The display name of the application. This name is displayed as the __Project name__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carName :: Lens.Lens' CreateApplicationRequest Lude.Text
carName = Lens.lens (name :: CreateApplicationRequest -> Lude.Text) (\s a -> s {name = a} :: CreateApplicationRequest)
{-# DEPRECATED carName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A string-to-string map of key-value pairs that defines the tags to associate with the application. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carTags :: Lens.Lens' CreateApplicationRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
carTags = Lens.lens (tags :: CreateApplicationRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateApplicationRequest)
{-# DEPRECATED carTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.ToJSON CreateApplicationRequest where
  toJSON CreateApplicationRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Name" Lude..= name), ("tags" Lude..=) Lude.<$> tags]
      )
