-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.CodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.CodeSource
  ( CodeSource (..),

    -- * Smart constructor
    mkCodeSource,

    -- * Lenses
    csS3,
  )
where

import Network.AWS.CodeStar.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
--
-- /See:/ 'mkCodeSource' smart constructor.
newtype CodeSource = CodeSource' {s3 :: S3Location}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeSource' with the minimum fields required to make a request.
--
-- * 's3' - Information about the Amazon S3 location where the source code files provided with the project request are stored.
mkCodeSource ::
  -- | 's3'
  S3Location ->
  CodeSource
mkCodeSource pS3_ = CodeSource' {s3 = pS3_}

-- | Information about the Amazon S3 location where the source code files provided with the project request are stored.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csS3 :: Lens.Lens' CodeSource S3Location
csS3 = Lens.lens (s3 :: CodeSource -> S3Location) (\s a -> s {s3 = a} :: CodeSource)
{-# DEPRECATED csS3 "Use generic-lens or generic-optics with 's3' instead." #-}

instance Lude.ToJSON CodeSource where
  toJSON CodeSource' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("s3" Lude..= s3)])
