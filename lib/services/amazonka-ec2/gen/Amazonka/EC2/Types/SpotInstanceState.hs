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
-- Module      : Amazonka.EC2.Types.SpotInstanceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotInstanceState
  ( SpotInstanceState
      ( ..,
        SpotInstanceState_Active,
        SpotInstanceState_Cancelled,
        SpotInstanceState_Closed,
        SpotInstanceState_Failed,
        SpotInstanceState_Open
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype SpotInstanceState = SpotInstanceState'
  { fromSpotInstanceState ::
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

pattern SpotInstanceState_Active :: SpotInstanceState
pattern SpotInstanceState_Active = SpotInstanceState' "active"

pattern SpotInstanceState_Cancelled :: SpotInstanceState
pattern SpotInstanceState_Cancelled = SpotInstanceState' "cancelled"

pattern SpotInstanceState_Closed :: SpotInstanceState
pattern SpotInstanceState_Closed = SpotInstanceState' "closed"

pattern SpotInstanceState_Failed :: SpotInstanceState
pattern SpotInstanceState_Failed = SpotInstanceState' "failed"

pattern SpotInstanceState_Open :: SpotInstanceState
pattern SpotInstanceState_Open = SpotInstanceState' "open"

{-# COMPLETE
  SpotInstanceState_Active,
  SpotInstanceState_Cancelled,
  SpotInstanceState_Closed,
  SpotInstanceState_Failed,
  SpotInstanceState_Open,
  SpotInstanceState'
  #-}
