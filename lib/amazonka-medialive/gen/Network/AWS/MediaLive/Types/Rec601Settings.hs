{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Rec601Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Rec601Settings
  ( Rec601Settings (..),

    -- * Smart constructor
    mkRec601Settings,

    -- * Lenses
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Rec601 Settings
--
-- /See:/ 'mkRec601Settings' smart constructor.
data Rec601Settings = Rec601Settings'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Rec601Settings' with the minimum fields required to make a request.
mkRec601Settings ::
  Rec601Settings
mkRec601Settings = Rec601Settings'

instance Lude.FromJSON Rec601Settings where
  parseJSON =
    Lude.withObject
      "Rec601Settings"
      (\x -> Lude.pure Rec601Settings')

instance Lude.ToJSON Rec601Settings where
  toJSON = Lude.const (Lude.Object Lude.mempty)
