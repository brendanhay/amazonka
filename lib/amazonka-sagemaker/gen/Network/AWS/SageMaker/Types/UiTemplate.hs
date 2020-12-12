{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiTemplate
  ( UiTemplate (..),

    -- * Smart constructor
    mkUiTemplate,

    -- * Lenses
    utContent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Liquid template for the worker user interface.
--
-- /See:/ 'mkUiTemplate' smart constructor.
newtype UiTemplate = UiTemplate' {content :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UiTemplate' with the minimum fields required to make a request.
--
-- * 'content' - The content of the Liquid template for the worker user interface.
mkUiTemplate ::
  -- | 'content'
  Lude.Text ->
  UiTemplate
mkUiTemplate pContent_ = UiTemplate' {content = pContent_}

-- | The content of the Liquid template for the worker user interface.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utContent :: Lens.Lens' UiTemplate Lude.Text
utContent = Lens.lens (content :: UiTemplate -> Lude.Text) (\s a -> s {content = a} :: UiTemplate)
{-# DEPRECATED utContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Lude.ToJSON UiTemplate where
  toJSON UiTemplate' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Content" Lude..= content)])
