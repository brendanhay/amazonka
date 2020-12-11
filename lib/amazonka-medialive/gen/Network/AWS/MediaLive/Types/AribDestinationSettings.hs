-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AribDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AribDestinationSettings
  ( AribDestinationSettings (..),

    -- * Smart constructor
    mkAribDestinationSettings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Arib Destination Settings
--
-- /See:/ 'mkAribDestinationSettings' smart constructor.
data AribDestinationSettings = AribDestinationSettings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AribDestinationSettings' with the minimum fields required to make a request.
mkAribDestinationSettings ::
  AribDestinationSettings
mkAribDestinationSettings = AribDestinationSettings'

instance Lude.FromJSON AribDestinationSettings where
  parseJSON =
    Lude.withObject
      "AribDestinationSettings"
      (\x -> Lude.pure AribDestinationSettings')

instance Lude.ToJSON AribDestinationSettings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
