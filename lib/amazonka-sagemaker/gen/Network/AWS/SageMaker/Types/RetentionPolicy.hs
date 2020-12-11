-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RetentionPolicy
  ( RetentionPolicy (..),

    -- * Smart constructor
    mkRetentionPolicy,

    -- * Lenses
    rpHomeEfsFileSystem,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.RetentionType

-- | The retention policy for data stored on an Amazon Elastic File System (EFS) volume.
--
-- /See:/ 'mkRetentionPolicy' smart constructor.
newtype RetentionPolicy = RetentionPolicy'
  { homeEfsFileSystem ::
      Lude.Maybe RetentionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetentionPolicy' with the minimum fields required to make a request.
--
-- * 'homeEfsFileSystem' - The default is @Retain@ , which specifies to keep the data stored on the EFS volume.
--
-- Specify @Delete@ to delete the data stored on the EFS volume.
mkRetentionPolicy ::
  RetentionPolicy
mkRetentionPolicy =
  RetentionPolicy' {homeEfsFileSystem = Lude.Nothing}

-- | The default is @Retain@ , which specifies to keep the data stored on the EFS volume.
--
-- Specify @Delete@ to delete the data stored on the EFS volume.
--
-- /Note:/ Consider using 'homeEfsFileSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpHomeEfsFileSystem :: Lens.Lens' RetentionPolicy (Lude.Maybe RetentionType)
rpHomeEfsFileSystem = Lens.lens (homeEfsFileSystem :: RetentionPolicy -> Lude.Maybe RetentionType) (\s a -> s {homeEfsFileSystem = a} :: RetentionPolicy)
{-# DEPRECATED rpHomeEfsFileSystem "Use generic-lens or generic-optics with 'homeEfsFileSystem' instead." #-}

instance Lude.ToJSON RetentionPolicy where
  toJSON RetentionPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [("HomeEfsFileSystem" Lude..=) Lude.<$> homeEfsFileSystem]
      )
