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
-- Module      : Amazonka.EC2.Types.State
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.State
  ( State
      ( ..,
        State_Available,
        State_Deleted,
        State_Deleting,
        State_Expired,
        State_Failed,
        State_Pending,
        State_PendingAcceptance,
        State_Rejected
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype State = State' {fromState :: Data.Text}
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

pattern State_Available :: State
pattern State_Available = State' "Available"

pattern State_Deleted :: State
pattern State_Deleted = State' "Deleted"

pattern State_Deleting :: State
pattern State_Deleting = State' "Deleting"

pattern State_Expired :: State
pattern State_Expired = State' "Expired"

pattern State_Failed :: State
pattern State_Failed = State' "Failed"

pattern State_Pending :: State
pattern State_Pending = State' "Pending"

pattern State_PendingAcceptance :: State
pattern State_PendingAcceptance = State' "PendingAcceptance"

pattern State_Rejected :: State
pattern State_Rejected = State' "Rejected"

{-# COMPLETE
  State_Available,
  State_Deleted,
  State_Deleting,
  State_Expired,
  State_Failed,
  State_Pending,
  State_PendingAcceptance,
  State_Rejected,
  State'
  #-}
