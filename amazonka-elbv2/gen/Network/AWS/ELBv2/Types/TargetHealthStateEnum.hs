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
-- Module      : Network.AWS.ELBv2.Types.TargetHealthStateEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealthStateEnum
  ( TargetHealthStateEnum
      ( ..,
        TargetHealthStateEnum_Draining,
        TargetHealthStateEnum_Healthy,
        TargetHealthStateEnum_Initial,
        TargetHealthStateEnum_Unavailable,
        TargetHealthStateEnum_Unhealthy,
        TargetHealthStateEnum_Unused
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TargetHealthStateEnum = TargetHealthStateEnum'
  { fromTargetHealthStateEnum ::
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

pattern TargetHealthStateEnum_Draining :: TargetHealthStateEnum
pattern TargetHealthStateEnum_Draining = TargetHealthStateEnum' "draining"

pattern TargetHealthStateEnum_Healthy :: TargetHealthStateEnum
pattern TargetHealthStateEnum_Healthy = TargetHealthStateEnum' "healthy"

pattern TargetHealthStateEnum_Initial :: TargetHealthStateEnum
pattern TargetHealthStateEnum_Initial = TargetHealthStateEnum' "initial"

pattern TargetHealthStateEnum_Unavailable :: TargetHealthStateEnum
pattern TargetHealthStateEnum_Unavailable = TargetHealthStateEnum' "unavailable"

pattern TargetHealthStateEnum_Unhealthy :: TargetHealthStateEnum
pattern TargetHealthStateEnum_Unhealthy = TargetHealthStateEnum' "unhealthy"

pattern TargetHealthStateEnum_Unused :: TargetHealthStateEnum
pattern TargetHealthStateEnum_Unused = TargetHealthStateEnum' "unused"

{-# COMPLETE
  TargetHealthStateEnum_Draining,
  TargetHealthStateEnum_Healthy,
  TargetHealthStateEnum_Initial,
  TargetHealthStateEnum_Unavailable,
  TargetHealthStateEnum_Unhealthy,
  TargetHealthStateEnum_Unused,
  TargetHealthStateEnum'
  #-}
