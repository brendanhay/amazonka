{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SourceFileSpecifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SourceFileSpecifier
  ( SourceFileSpecifier (..),

    -- * Smart constructor
    mkSourceFileSpecifier,

    -- * Lenses
    sfsFilePath,
    sfsIsMove,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a source file that is part of changes made in a commit.
--
-- /See:/ 'mkSourceFileSpecifier' smart constructor.
data SourceFileSpecifier = SourceFileSpecifier'
  { -- | The full path to the file, including the name of the file.
    filePath :: Lude.Text,
    -- | Whether to remove the source file from the parent commit.
    isMove :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceFileSpecifier' with the minimum fields required to make a request.
--
-- * 'filePath' - The full path to the file, including the name of the file.
-- * 'isMove' - Whether to remove the source file from the parent commit.
mkSourceFileSpecifier ::
  -- | 'filePath'
  Lude.Text ->
  SourceFileSpecifier
mkSourceFileSpecifier pFilePath_ =
  SourceFileSpecifier'
    { filePath = pFilePath_,
      isMove = Lude.Nothing
    }

-- | The full path to the file, including the name of the file.
--
-- /Note:/ Consider using 'filePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsFilePath :: Lens.Lens' SourceFileSpecifier Lude.Text
sfsFilePath = Lens.lens (filePath :: SourceFileSpecifier -> Lude.Text) (\s a -> s {filePath = a} :: SourceFileSpecifier)
{-# DEPRECATED sfsFilePath "Use generic-lens or generic-optics with 'filePath' instead." #-}

-- | Whether to remove the source file from the parent commit.
--
-- /Note:/ Consider using 'isMove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfsIsMove :: Lens.Lens' SourceFileSpecifier (Lude.Maybe Lude.Bool)
sfsIsMove = Lens.lens (isMove :: SourceFileSpecifier -> Lude.Maybe Lude.Bool) (\s a -> s {isMove = a} :: SourceFileSpecifier)
{-# DEPRECATED sfsIsMove "Use generic-lens or generic-optics with 'isMove' instead." #-}

instance Lude.ToJSON SourceFileSpecifier where
  toJSON SourceFileSpecifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("filePath" Lude..= filePath),
            ("isMove" Lude..=) Lude.<$> isMove
          ]
      )
