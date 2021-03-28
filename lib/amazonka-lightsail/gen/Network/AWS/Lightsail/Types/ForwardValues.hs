{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ForwardValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ForwardValues
  ( ForwardValues
    ( ForwardValues'
    , ForwardValuesNone
    , ForwardValuesAllowList
    , ForwardValuesAll
    , fromForwardValues
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ForwardValues = ForwardValues'{fromForwardValues ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern ForwardValuesNone :: ForwardValues
pattern ForwardValuesNone = ForwardValues' "none"

pattern ForwardValuesAllowList :: ForwardValues
pattern ForwardValuesAllowList = ForwardValues' "allow-list"

pattern ForwardValuesAll :: ForwardValues
pattern ForwardValuesAll = ForwardValues' "all"

{-# COMPLETE 
  ForwardValuesNone,

  ForwardValuesAllowList,

  ForwardValuesAll,
  ForwardValues'
  #-}
