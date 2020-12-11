-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ToolchainSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.ToolchainSource
  ( ToolchainSource (..),

    -- * Smart constructor
    mkToolchainSource,

    -- * Lenses
    tsS3,
  )
where

import Network.AWS.CodeStar.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
--
-- /See:/ 'mkToolchainSource' smart constructor.
newtype ToolchainSource = ToolchainSource' {s3 :: S3Location}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ToolchainSource' with the minimum fields required to make a request.
--
-- * 's3' - The Amazon S3 bucket where the toolchain template file provided with the project request is stored.
mkToolchainSource ::
  -- | 's3'
  S3Location ->
  ToolchainSource
mkToolchainSource pS3_ = ToolchainSource' {s3 = pS3_}

-- | The Amazon S3 bucket where the toolchain template file provided with the project request is stored.
--
-- /Note:/ Consider using 's3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsS3 :: Lens.Lens' ToolchainSource S3Location
tsS3 = Lens.lens (s3 :: ToolchainSource -> S3Location) (\s a -> s {s3 = a} :: ToolchainSource)
{-# DEPRECATED tsS3 "Use generic-lens or generic-optics with 's3' instead." #-}

instance Lude.ToJSON ToolchainSource where
  toJSON ToolchainSource' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("s3" Lude..= s3)])
