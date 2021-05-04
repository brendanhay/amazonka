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
-- Module      : Network.AWS.RDS.Types.TargetState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetState
  ( TargetState
      ( ..,
        TargetState_AVAILABLE,
        TargetState_REGISTERING,
        TargetState_UNAVAILABLE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TargetState = TargetState'
  { fromTargetState ::
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

pattern TargetState_AVAILABLE :: TargetState
pattern TargetState_AVAILABLE = TargetState' "AVAILABLE"

pattern TargetState_REGISTERING :: TargetState
pattern TargetState_REGISTERING = TargetState' "REGISTERING"

pattern TargetState_UNAVAILABLE :: TargetState
pattern TargetState_UNAVAILABLE = TargetState' "UNAVAILABLE"

{-# COMPLETE
  TargetState_AVAILABLE,
  TargetState_REGISTERING,
  TargetState_UNAVAILABLE,
  TargetState'
  #-}
