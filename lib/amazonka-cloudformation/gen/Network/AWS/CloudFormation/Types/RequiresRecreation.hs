{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RequiresRecreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.RequiresRecreation
  ( RequiresRecreation
    ( RequiresRecreation'
    , RequiresRecreationNever
    , RequiresRecreationConditionally
    , RequiresRecreationAlways
    , fromRequiresRecreation
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RequiresRecreation = RequiresRecreation'{fromRequiresRecreation
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern RequiresRecreationNever :: RequiresRecreation
pattern RequiresRecreationNever = RequiresRecreation' "Never"

pattern RequiresRecreationConditionally :: RequiresRecreation
pattern RequiresRecreationConditionally = RequiresRecreation' "Conditionally"

pattern RequiresRecreationAlways :: RequiresRecreation
pattern RequiresRecreationAlways = RequiresRecreation' "Always"

{-# COMPLETE 
  RequiresRecreationNever,

  RequiresRecreationConditionally,

  RequiresRecreationAlways,
  RequiresRecreation'
  #-}
