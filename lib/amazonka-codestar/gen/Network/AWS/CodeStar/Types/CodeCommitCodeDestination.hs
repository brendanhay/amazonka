{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.CodeCommitCodeDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.CodeCommitCodeDestination
  ( CodeCommitCodeDestination (..),

    -- * Smart constructor
    mkCodeCommitCodeDestination,

    -- * Lenses
    cccdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /See:/ 'mkCodeCommitCodeDestination' smart constructor.
newtype CodeCommitCodeDestination = CodeCommitCodeDestination'
  { name ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeCommitCodeDestination' with the minimum fields required to make a request.
--
-- * 'name' - The name of the AWS CodeCommit repository to be created in AWS CodeStar.
mkCodeCommitCodeDestination ::
  -- | 'name'
  Lude.Text ->
  CodeCommitCodeDestination
mkCodeCommitCodeDestination pName_ =
  CodeCommitCodeDestination' {name = pName_}

-- | The name of the AWS CodeCommit repository to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdName :: Lens.Lens' CodeCommitCodeDestination Lude.Text
cccdName = Lens.lens (name :: CodeCommitCodeDestination -> Lude.Text) (\s a -> s {name = a} :: CodeCommitCodeDestination)
{-# DEPRECATED cccdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON CodeCommitCodeDestination where
  toJSON CodeCommitCodeDestination' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])
