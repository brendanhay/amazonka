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
-- Module      : Amazonka.SES.Types.DsnAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.DsnAction
  ( DsnAction
      ( ..,
        DsnAction_Delayed,
        DsnAction_Delivered,
        DsnAction_Expanded,
        DsnAction_Failed,
        DsnAction_Relayed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DsnAction = DsnAction'
  { fromDsnAction ::
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

pattern DsnAction_Delayed :: DsnAction
pattern DsnAction_Delayed = DsnAction' "delayed"

pattern DsnAction_Delivered :: DsnAction
pattern DsnAction_Delivered = DsnAction' "delivered"

pattern DsnAction_Expanded :: DsnAction
pattern DsnAction_Expanded = DsnAction' "expanded"

pattern DsnAction_Failed :: DsnAction
pattern DsnAction_Failed = DsnAction' "failed"

pattern DsnAction_Relayed :: DsnAction
pattern DsnAction_Relayed = DsnAction' "relayed"

{-# COMPLETE
  DsnAction_Delayed,
  DsnAction_Delivered,
  DsnAction_Expanded,
  DsnAction_Failed,
  DsnAction_Relayed,
  DsnAction'
  #-}
