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
-- Module      : Network.AWS.IoT.Types.DynamicGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DynamicGroupStatus
  ( DynamicGroupStatus
      ( ..,
        DynamicGroupStatus_ACTIVE,
        DynamicGroupStatus_BUILDING,
        DynamicGroupStatus_REBUILDING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DynamicGroupStatus = DynamicGroupStatus'
  { fromDynamicGroupStatus ::
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

pattern DynamicGroupStatus_ACTIVE :: DynamicGroupStatus
pattern DynamicGroupStatus_ACTIVE = DynamicGroupStatus' "ACTIVE"

pattern DynamicGroupStatus_BUILDING :: DynamicGroupStatus
pattern DynamicGroupStatus_BUILDING = DynamicGroupStatus' "BUILDING"

pattern DynamicGroupStatus_REBUILDING :: DynamicGroupStatus
pattern DynamicGroupStatus_REBUILDING = DynamicGroupStatus' "REBUILDING"

{-# COMPLETE
  DynamicGroupStatus_ACTIVE,
  DynamicGroupStatus_BUILDING,
  DynamicGroupStatus_REBUILDING,
  DynamicGroupStatus'
  #-}
