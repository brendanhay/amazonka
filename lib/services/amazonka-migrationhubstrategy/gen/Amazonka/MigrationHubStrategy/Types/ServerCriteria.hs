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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerCriteria
  ( ServerCriteria
      ( ..,
        ServerCriteria_DESTINATION,
        ServerCriteria_NOT_DEFINED,
        ServerCriteria_OS_NAME,
        ServerCriteria_SERVER_ID,
        ServerCriteria_STRATEGY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ServerCriteria = ServerCriteria'
  { fromServerCriteria ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ServerCriteria_DESTINATION :: ServerCriteria
pattern ServerCriteria_DESTINATION = ServerCriteria' "DESTINATION"

pattern ServerCriteria_NOT_DEFINED :: ServerCriteria
pattern ServerCriteria_NOT_DEFINED = ServerCriteria' "NOT_DEFINED"

pattern ServerCriteria_OS_NAME :: ServerCriteria
pattern ServerCriteria_OS_NAME = ServerCriteria' "OS_NAME"

pattern ServerCriteria_SERVER_ID :: ServerCriteria
pattern ServerCriteria_SERVER_ID = ServerCriteria' "SERVER_ID"

pattern ServerCriteria_STRATEGY :: ServerCriteria
pattern ServerCriteria_STRATEGY = ServerCriteria' "STRATEGY"

{-# COMPLETE
  ServerCriteria_DESTINATION,
  ServerCriteria_NOT_DEFINED,
  ServerCriteria_OS_NAME,
  ServerCriteria_SERVER_ID,
  ServerCriteria_STRATEGY,
  ServerCriteria'
  #-}
