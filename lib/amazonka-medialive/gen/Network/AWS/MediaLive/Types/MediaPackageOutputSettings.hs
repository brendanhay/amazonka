{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageOutputSettings
  ( MediaPackageOutputSettings (..),

    -- * Smart constructor
    mkMediaPackageOutputSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Media Package Output Settings
--
-- /See:/ 'mkMediaPackageOutputSettings' smart constructor.
data MediaPackageOutputSettings = MediaPackageOutputSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MediaPackageOutputSettings' with the minimum fields required to make a request.
mkMediaPackageOutputSettings ::
  MediaPackageOutputSettings
mkMediaPackageOutputSettings = MediaPackageOutputSettings'

instance Lude.FromJSON MediaPackageOutputSettings where
  parseJSON =
    Lude.withObject
      "MediaPackageOutputSettings"
      (\x -> Lude.pure MediaPackageOutputSettings')

instance Lude.ToJSON MediaPackageOutputSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
