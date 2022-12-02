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
-- Module      : Amazonka.MigrationHubStrategy.Types.ApplicationComponentCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ApplicationComponentCriteria
  ( ApplicationComponentCriteria
      ( ..,
        ApplicationComponentCriteria_APP_NAME,
        ApplicationComponentCriteria_APP_TYPE,
        ApplicationComponentCriteria_DESTINATION,
        ApplicationComponentCriteria_NOT_DEFINED,
        ApplicationComponentCriteria_SERVER_ID,
        ApplicationComponentCriteria_STRATEGY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationComponentCriteria = ApplicationComponentCriteria'
  { fromApplicationComponentCriteria ::
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

pattern ApplicationComponentCriteria_APP_NAME :: ApplicationComponentCriteria
pattern ApplicationComponentCriteria_APP_NAME = ApplicationComponentCriteria' "APP_NAME"

pattern ApplicationComponentCriteria_APP_TYPE :: ApplicationComponentCriteria
pattern ApplicationComponentCriteria_APP_TYPE = ApplicationComponentCriteria' "APP_TYPE"

pattern ApplicationComponentCriteria_DESTINATION :: ApplicationComponentCriteria
pattern ApplicationComponentCriteria_DESTINATION = ApplicationComponentCriteria' "DESTINATION"

pattern ApplicationComponentCriteria_NOT_DEFINED :: ApplicationComponentCriteria
pattern ApplicationComponentCriteria_NOT_DEFINED = ApplicationComponentCriteria' "NOT_DEFINED"

pattern ApplicationComponentCriteria_SERVER_ID :: ApplicationComponentCriteria
pattern ApplicationComponentCriteria_SERVER_ID = ApplicationComponentCriteria' "SERVER_ID"

pattern ApplicationComponentCriteria_STRATEGY :: ApplicationComponentCriteria
pattern ApplicationComponentCriteria_STRATEGY = ApplicationComponentCriteria' "STRATEGY"

{-# COMPLETE
  ApplicationComponentCriteria_APP_NAME,
  ApplicationComponentCriteria_APP_TYPE,
  ApplicationComponentCriteria_DESTINATION,
  ApplicationComponentCriteria_NOT_DEFINED,
  ApplicationComponentCriteria_SERVER_ID,
  ApplicationComponentCriteria_STRATEGY,
  ApplicationComponentCriteria'
  #-}
