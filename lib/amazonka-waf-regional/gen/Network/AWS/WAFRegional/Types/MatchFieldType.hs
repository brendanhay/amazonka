{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.MatchFieldType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.MatchFieldType
  ( MatchFieldType
    ( MatchFieldType'
    , MatchFieldTypeUri
    , MatchFieldTypeQueryString
    , MatchFieldTypeHeader
    , MatchFieldTypeMethod
    , MatchFieldTypeBody
    , MatchFieldTypeSingleQueryArg
    , MatchFieldTypeAllQueryArgs
    , fromMatchFieldType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MatchFieldType = MatchFieldType'{fromMatchFieldType ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern MatchFieldTypeUri :: MatchFieldType
pattern MatchFieldTypeUri = MatchFieldType' "URI"

pattern MatchFieldTypeQueryString :: MatchFieldType
pattern MatchFieldTypeQueryString = MatchFieldType' "QUERY_STRING"

pattern MatchFieldTypeHeader :: MatchFieldType
pattern MatchFieldTypeHeader = MatchFieldType' "HEADER"

pattern MatchFieldTypeMethod :: MatchFieldType
pattern MatchFieldTypeMethod = MatchFieldType' "METHOD"

pattern MatchFieldTypeBody :: MatchFieldType
pattern MatchFieldTypeBody = MatchFieldType' "BODY"

pattern MatchFieldTypeSingleQueryArg :: MatchFieldType
pattern MatchFieldTypeSingleQueryArg = MatchFieldType' "SINGLE_QUERY_ARG"

pattern MatchFieldTypeAllQueryArgs :: MatchFieldType
pattern MatchFieldTypeAllQueryArgs = MatchFieldType' "ALL_QUERY_ARGS"

{-# COMPLETE 
  MatchFieldTypeUri,

  MatchFieldTypeQueryString,

  MatchFieldTypeHeader,

  MatchFieldTypeMethod,

  MatchFieldTypeBody,

  MatchFieldTypeSingleQueryArg,

  MatchFieldTypeAllQueryArgs,
  MatchFieldType'
  #-}
