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
-- Module      : Network.AWS.SES.Types.DsnAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.DsnAction
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DsnAction = DsnAction'
  { fromDsnAction ::
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
