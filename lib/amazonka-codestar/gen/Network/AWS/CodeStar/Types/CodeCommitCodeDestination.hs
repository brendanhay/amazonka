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

import qualified Network.AWS.CodeStar.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- /See:/ 'mkCodeCommitCodeDestination' smart constructor.
newtype CodeCommitCodeDestination = CodeCommitCodeDestination'
  { -- | The name of the AWS CodeCommit repository to be created in AWS CodeStar.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CodeCommitCodeDestination' value with any optional fields omitted.
mkCodeCommitCodeDestination ::
  -- | 'name'
  Types.Name ->
  CodeCommitCodeDestination
mkCodeCommitCodeDestination name = CodeCommitCodeDestination' {name}

-- | The name of the AWS CodeCommit repository to be created in AWS CodeStar.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccdName :: Lens.Lens' CodeCommitCodeDestination Types.Name
cccdName = Lens.field @"name"
{-# DEPRECATED cccdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON CodeCommitCodeDestination where
  toJSON CodeCommitCodeDestination {..} =
    Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])
