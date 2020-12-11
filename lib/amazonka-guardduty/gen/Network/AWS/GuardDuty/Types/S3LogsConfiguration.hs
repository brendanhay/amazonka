-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.S3LogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3LogsConfiguration
  ( S3LogsConfiguration (..),

    -- * Smart constructor
    mkS3LogsConfiguration,

    -- * Lenses
    slcEnable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes whether S3 data event logs will be enabled as a data source.
--
-- /See:/ 'mkS3LogsConfiguration' smart constructor.
newtype S3LogsConfiguration = S3LogsConfiguration'
  { enable ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3LogsConfiguration' with the minimum fields required to make a request.
--
-- * 'enable' - The status of S3 data event logs as a data source.
mkS3LogsConfiguration ::
  -- | 'enable'
  Lude.Bool ->
  S3LogsConfiguration
mkS3LogsConfiguration pEnable_ =
  S3LogsConfiguration' {enable = pEnable_}

-- | The status of S3 data event logs as a data source.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcEnable :: Lens.Lens' S3LogsConfiguration Lude.Bool
slcEnable = Lens.lens (enable :: S3LogsConfiguration -> Lude.Bool) (\s a -> s {enable = a} :: S3LogsConfiguration)
{-# DEPRECATED slcEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

instance Lude.ToJSON S3LogsConfiguration where
  toJSON S3LogsConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("enable" Lude..= enable)])
