{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.MatchFieldType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.MatchFieldType
  ( MatchFieldType
      ( MatchFieldType',
        URI,
        QueryString,
        Header,
        Method,
        Body,
        SingleQueryArg,
        AllQueryArgs
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MatchFieldType = MatchFieldType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern URI :: MatchFieldType
pattern URI = MatchFieldType' "URI"

pattern QueryString :: MatchFieldType
pattern QueryString = MatchFieldType' "QUERY_STRING"

pattern Header :: MatchFieldType
pattern Header = MatchFieldType' "HEADER"

pattern Method :: MatchFieldType
pattern Method = MatchFieldType' "METHOD"

pattern Body :: MatchFieldType
pattern Body = MatchFieldType' "BODY"

pattern SingleQueryArg :: MatchFieldType
pattern SingleQueryArg = MatchFieldType' "SINGLE_QUERY_ARG"

pattern AllQueryArgs :: MatchFieldType
pattern AllQueryArgs = MatchFieldType' "ALL_QUERY_ARGS"

{-# COMPLETE
  URI,
  QueryString,
  Header,
  Method,
  Body,
  SingleQueryArg,
  AllQueryArgs,
  MatchFieldType'
  #-}
