{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Compatibility
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Compatibility
  ( Compatibility
    ( Compatibility'
    , CompatibilityNone
    , CompatibilityDisabled
    , CompatibilityBackward
    , CompatibilityBackwardAll
    , CompatibilityForward
    , CompatibilityForwardAll
    , CompatibilityFull
    , CompatibilityFullAll
    , fromCompatibility
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Compatibility = Compatibility'{fromCompatibility ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern CompatibilityNone :: Compatibility
pattern CompatibilityNone = Compatibility' "NONE"

pattern CompatibilityDisabled :: Compatibility
pattern CompatibilityDisabled = Compatibility' "DISABLED"

pattern CompatibilityBackward :: Compatibility
pattern CompatibilityBackward = Compatibility' "BACKWARD"

pattern CompatibilityBackwardAll :: Compatibility
pattern CompatibilityBackwardAll = Compatibility' "BACKWARD_ALL"

pattern CompatibilityForward :: Compatibility
pattern CompatibilityForward = Compatibility' "FORWARD"

pattern CompatibilityForwardAll :: Compatibility
pattern CompatibilityForwardAll = Compatibility' "FORWARD_ALL"

pattern CompatibilityFull :: Compatibility
pattern CompatibilityFull = Compatibility' "FULL"

pattern CompatibilityFullAll :: Compatibility
pattern CompatibilityFullAll = Compatibility' "FULL_ALL"

{-# COMPLETE 
  CompatibilityNone,

  CompatibilityDisabled,

  CompatibilityBackward,

  CompatibilityBackwardAll,

  CompatibilityForward,

  CompatibilityForwardAll,

  CompatibilityFull,

  CompatibilityFullAll,
  Compatibility'
  #-}
