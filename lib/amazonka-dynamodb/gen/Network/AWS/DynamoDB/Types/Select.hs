{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Select
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.Select
  ( Select
    ( Select'
    , SelectAllAttributes
    , SelectAllProjectedAttributes
    , SelectSpecificAttributes
    , SelectCount
    , fromSelect
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Select = Select'{fromSelect :: Core.Text}
                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                   Core.Generic)
                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                     Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                     Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern SelectAllAttributes :: Select
pattern SelectAllAttributes = Select' "ALL_ATTRIBUTES"

pattern SelectAllProjectedAttributes :: Select
pattern SelectAllProjectedAttributes = Select' "ALL_PROJECTED_ATTRIBUTES"

pattern SelectSpecificAttributes :: Select
pattern SelectSpecificAttributes = Select' "SPECIFIC_ATTRIBUTES"

pattern SelectCount :: Select
pattern SelectCount = Select' "COUNT"

{-# COMPLETE 
  SelectAllAttributes,

  SelectAllProjectedAttributes,

  SelectSpecificAttributes,

  SelectCount,
  Select'
  #-}
