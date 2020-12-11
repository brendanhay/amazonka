-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.QueryParser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.QueryParser
  ( QueryParser
      ( QueryParser',
        Dismax,
        Lucene,
        Simple,
        Structured
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype QueryParser = QueryParser' Lude.Text
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

pattern Dismax :: QueryParser
pattern Dismax = QueryParser' "dismax"

pattern Lucene :: QueryParser
pattern Lucene = QueryParser' "lucene"

pattern Simple :: QueryParser
pattern Simple = QueryParser' "simple"

pattern Structured :: QueryParser
pattern Structured = QueryParser' "structured"

{-# COMPLETE
  Dismax,
  Lucene,
  Simple,
  Structured,
  QueryParser'
  #-}
