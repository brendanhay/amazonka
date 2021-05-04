{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceState
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype SpotInstanceState = SpotInstanceState'
  { fromSpotInstanceState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
