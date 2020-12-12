{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildNotDeleted
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildNotDeleted
  ( BuildNotDeleted (..),

    -- * Smart constructor
    mkBuildNotDeleted,

    -- * Lenses
    bndId,
    bndStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a build that could not be successfully deleted.
--
-- /See:/ 'mkBuildNotDeleted' smart constructor.
data BuildNotDeleted = BuildNotDeleted'
  { id :: Lude.Maybe Lude.Text,
    statusCode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildNotDeleted' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the build that could not be successfully deleted.
-- * 'statusCode' - Additional information about the build that could not be successfully deleted.
mkBuildNotDeleted ::
  BuildNotDeleted
mkBuildNotDeleted =
  BuildNotDeleted' {id = Lude.Nothing, statusCode = Lude.Nothing}

-- | The ID of the build that could not be successfully deleted.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bndId :: Lens.Lens' BuildNotDeleted (Lude.Maybe Lude.Text)
bndId = Lens.lens (id :: BuildNotDeleted -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: BuildNotDeleted)
{-# DEPRECATED bndId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Additional information about the build that could not be successfully deleted.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bndStatusCode :: Lens.Lens' BuildNotDeleted (Lude.Maybe Lude.Text)
bndStatusCode = Lens.lens (statusCode :: BuildNotDeleted -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: BuildNotDeleted)
{-# DEPRECATED bndStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON BuildNotDeleted where
  parseJSON =
    Lude.withObject
      "BuildNotDeleted"
      ( \x ->
          BuildNotDeleted'
            Lude.<$> (x Lude..:? "id") Lude.<*> (x Lude..:? "statusCode")
      )
