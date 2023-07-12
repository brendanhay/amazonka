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
-- Module      : Amazonka.IoT.Types.LogTargetType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.LogTargetType
  ( LogTargetType
      ( ..,
        LogTargetType_CLIENT_ID,
        LogTargetType_DEFAULT,
        LogTargetType_PRINCIPAL_ID,
        LogTargetType_SOURCE_IP,
        LogTargetType_THING_GROUP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LogTargetType = LogTargetType'
  { fromLogTargetType ::
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

pattern LogTargetType_CLIENT_ID :: LogTargetType
pattern LogTargetType_CLIENT_ID = LogTargetType' "CLIENT_ID"

pattern LogTargetType_DEFAULT :: LogTargetType
pattern LogTargetType_DEFAULT = LogTargetType' "DEFAULT"

pattern LogTargetType_PRINCIPAL_ID :: LogTargetType
pattern LogTargetType_PRINCIPAL_ID = LogTargetType' "PRINCIPAL_ID"

pattern LogTargetType_SOURCE_IP :: LogTargetType
pattern LogTargetType_SOURCE_IP = LogTargetType' "SOURCE_IP"

pattern LogTargetType_THING_GROUP :: LogTargetType
pattern LogTargetType_THING_GROUP = LogTargetType' "THING_GROUP"

{-# COMPLETE
  LogTargetType_CLIENT_ID,
  LogTargetType_DEFAULT,
  LogTargetType_PRINCIPAL_ID,
  LogTargetType_SOURCE_IP,
  LogTargetType_THING_GROUP,
  LogTargetType'
  #-}
