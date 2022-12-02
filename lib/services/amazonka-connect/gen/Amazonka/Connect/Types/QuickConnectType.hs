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
-- Module      : Amazonka.Connect.Types.QuickConnectType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QuickConnectType
  ( QuickConnectType
      ( ..,
        QuickConnectType_PHONE_NUMBER,
        QuickConnectType_QUEUE,
        QuickConnectType_USER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QuickConnectType = QuickConnectType'
  { fromQuickConnectType ::
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

pattern QuickConnectType_PHONE_NUMBER :: QuickConnectType
pattern QuickConnectType_PHONE_NUMBER = QuickConnectType' "PHONE_NUMBER"

pattern QuickConnectType_QUEUE :: QuickConnectType
pattern QuickConnectType_QUEUE = QuickConnectType' "QUEUE"

pattern QuickConnectType_USER :: QuickConnectType
pattern QuickConnectType_USER = QuickConnectType' "USER"

{-# COMPLETE
  QuickConnectType_PHONE_NUMBER,
  QuickConnectType_QUEUE,
  QuickConnectType_USER,
  QuickConnectType'
  #-}
