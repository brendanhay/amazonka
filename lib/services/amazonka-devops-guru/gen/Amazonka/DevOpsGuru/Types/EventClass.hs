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
-- Module      : Amazonka.DevOpsGuru.Types.EventClass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.EventClass
  ( EventClass
      ( ..,
        EventClass_CONFIG_CHANGE,
        EventClass_DEPLOYMENT,
        EventClass_INFRASTRUCTURE,
        EventClass_SCHEMA_CHANGE,
        EventClass_SECURITY_CHANGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventClass = EventClass'
  { fromEventClass ::
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

pattern EventClass_CONFIG_CHANGE :: EventClass
pattern EventClass_CONFIG_CHANGE = EventClass' "CONFIG_CHANGE"

pattern EventClass_DEPLOYMENT :: EventClass
pattern EventClass_DEPLOYMENT = EventClass' "DEPLOYMENT"

pattern EventClass_INFRASTRUCTURE :: EventClass
pattern EventClass_INFRASTRUCTURE = EventClass' "INFRASTRUCTURE"

pattern EventClass_SCHEMA_CHANGE :: EventClass
pattern EventClass_SCHEMA_CHANGE = EventClass' "SCHEMA_CHANGE"

pattern EventClass_SECURITY_CHANGE :: EventClass
pattern EventClass_SECURITY_CHANGE = EventClass' "SECURITY_CHANGE"

{-# COMPLETE
  EventClass_CONFIG_CHANGE,
  EventClass_DEPLOYMENT,
  EventClass_INFRASTRUCTURE,
  EventClass_SCHEMA_CHANGE,
  EventClass_SECURITY_CHANGE,
  EventClass'
  #-}
