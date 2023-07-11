{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudSearchDomains.Types.QueryParser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.QueryParser
  ( QueryParser
      ( ..,
        QueryParser_Dismax,
        QueryParser_Lucene,
        QueryParser_Simple,
        QueryParser_Structured
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QueryParser = QueryParser'
  { fromQueryParser ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
