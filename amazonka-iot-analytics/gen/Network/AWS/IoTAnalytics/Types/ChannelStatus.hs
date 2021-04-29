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
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelStatus
  ( ChannelStatus
      ( ..,
        ChannelStatus_ACTIVE,
        ChannelStatus_CREATING,
        ChannelStatus_DELETING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ChannelStatus = ChannelStatus'
  { fromChannelStatus ::
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

pattern ChannelStatus_ACTIVE :: ChannelStatus
pattern ChannelStatus_ACTIVE = ChannelStatus' "ACTIVE"

pattern ChannelStatus_CREATING :: ChannelStatus
pattern ChannelStatus_CREATING = ChannelStatus' "CREATING"

pattern ChannelStatus_DELETING :: ChannelStatus
pattern ChannelStatus_DELETING = ChannelStatus' "DELETING"

{-# COMPLETE
  ChannelStatus_ACTIVE,
  ChannelStatus_CREATING,
  ChannelStatus_DELETING,
  ChannelStatus'
  #-}
