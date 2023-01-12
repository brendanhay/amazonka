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
-- Module      : Amazonka.MigrationHubStrategy.Types.ServerCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerCriteria
  ( ServerCriteria
      ( ..,
        ServerCriteria_ANALYSIS_STATUS,
        ServerCriteria_DESTINATION,
        ServerCriteria_ERROR_CATEGORY,
        ServerCriteria_NOT_DEFINED,
        ServerCriteria_OS_NAME,
        ServerCriteria_SERVER_ID,
        ServerCriteria_STRATEGY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServerCriteria = ServerCriteria'
  { fromServerCriteria ::
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

pattern ServerCriteria_ANALYSIS_STATUS :: ServerCriteria
pattern ServerCriteria_ANALYSIS_STATUS = ServerCriteria' "ANALYSIS_STATUS"

pattern ServerCriteria_DESTINATION :: ServerCriteria
pattern ServerCriteria_DESTINATION = ServerCriteria' "DESTINATION"

pattern ServerCriteria_ERROR_CATEGORY :: ServerCriteria
pattern ServerCriteria_ERROR_CATEGORY = ServerCriteria' "ERROR_CATEGORY"

pattern ServerCriteria_NOT_DEFINED :: ServerCriteria
pattern ServerCriteria_NOT_DEFINED = ServerCriteria' "NOT_DEFINED"

pattern ServerCriteria_OS_NAME :: ServerCriteria
pattern ServerCriteria_OS_NAME = ServerCriteria' "OS_NAME"

pattern ServerCriteria_SERVER_ID :: ServerCriteria
pattern ServerCriteria_SERVER_ID = ServerCriteria' "SERVER_ID"

pattern ServerCriteria_STRATEGY :: ServerCriteria
pattern ServerCriteria_STRATEGY = ServerCriteria' "STRATEGY"

{-# COMPLETE
  ServerCriteria_ANALYSIS_STATUS,
  ServerCriteria_DESTINATION,
  ServerCriteria_ERROR_CATEGORY,
  ServerCriteria_NOT_DEFINED,
  ServerCriteria_OS_NAME,
  ServerCriteria_SERVER_ID,
  ServerCriteria_STRATEGY,
  ServerCriteria'
  #-}
