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
-- Module      : Amazonka.AppConfig.Types.TriggeredBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.TriggeredBy
  ( TriggeredBy
      ( ..,
        TriggeredBy_APPCONFIG,
        TriggeredBy_CLOUDWATCH_ALARM,
        TriggeredBy_INTERNAL_ERROR,
        TriggeredBy_USER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TriggeredBy = TriggeredBy'
  { fromTriggeredBy ::
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

pattern TriggeredBy_APPCONFIG :: TriggeredBy
pattern TriggeredBy_APPCONFIG = TriggeredBy' "APPCONFIG"

pattern TriggeredBy_CLOUDWATCH_ALARM :: TriggeredBy
pattern TriggeredBy_CLOUDWATCH_ALARM = TriggeredBy' "CLOUDWATCH_ALARM"

pattern TriggeredBy_INTERNAL_ERROR :: TriggeredBy
pattern TriggeredBy_INTERNAL_ERROR = TriggeredBy' "INTERNAL_ERROR"

pattern TriggeredBy_USER :: TriggeredBy
pattern TriggeredBy_USER = TriggeredBy' "USER"

{-# COMPLETE
  TriggeredBy_APPCONFIG,
  TriggeredBy_CLOUDWATCH_ALARM,
  TriggeredBy_INTERNAL_ERROR,
  TriggeredBy_USER,
  TriggeredBy'
  #-}
