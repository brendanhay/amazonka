{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.QueryParser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearchDomains.Types.QueryParser
  ( QueryParser
    ( QueryParser'
    , QueryParserSimple
    , QueryParserStructured
    , QueryParserLucene
    , QueryParserDismax
    , fromQueryParser
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype QueryParser = QueryParser'{fromQueryParser :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern QueryParserSimple :: QueryParser
pattern QueryParserSimple = QueryParser' "simple"

pattern QueryParserStructured :: QueryParser
pattern QueryParserStructured = QueryParser' "structured"

pattern QueryParserLucene :: QueryParser
pattern QueryParserLucene = QueryParser' "lucene"

pattern QueryParserDismax :: QueryParser
pattern QueryParserDismax = QueryParser' "dismax"

{-# COMPLETE 
  QueryParserSimple,

  QueryParserStructured,

  QueryParserLucene,

  QueryParserDismax,
  QueryParser'
  #-}
