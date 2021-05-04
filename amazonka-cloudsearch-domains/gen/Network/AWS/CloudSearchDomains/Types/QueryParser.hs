{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.QueryParser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.QueryParser
  ( QueryParser
      ( ..,
        QueryParser_Dismax,
        QueryParser_Lucene,
        QueryParser_Simple,
        QueryParser_Structured
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype QueryParser = QueryParser'
  { fromQueryParser ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern QueryParser_Dismax :: QueryParser
pattern QueryParser_Dismax = QueryParser' "dismax"

pattern QueryParser_Lucene :: QueryParser
pattern QueryParser_Lucene = QueryParser' "lucene"

pattern QueryParser_Simple :: QueryParser
pattern QueryParser_Simple = QueryParser' "simple"

pattern QueryParser_Structured :: QueryParser
pattern QueryParser_Structured = QueryParser' "structured"

{-# COMPLETE
  QueryParser_Dismax,
  QueryParser_Lucene,
  QueryParser_Simple,
  QueryParser_Structured,
  QueryParser'
  #-}
