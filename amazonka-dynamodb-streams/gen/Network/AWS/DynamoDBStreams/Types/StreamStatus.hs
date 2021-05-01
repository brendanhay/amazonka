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
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.StreamStatus
  ( StreamStatus
      ( ..,
        StreamStatus_DISABLED,
        StreamStatus_DISABLING,
        StreamStatus_ENABLED,
        StreamStatus_ENABLING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StreamStatus = StreamStatus'
  { fromStreamStatus ::
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

pattern StreamStatus_DISABLED :: StreamStatus
pattern StreamStatus_DISABLED = StreamStatus' "DISABLED"

pattern StreamStatus_DISABLING :: StreamStatus
pattern StreamStatus_DISABLING = StreamStatus' "DISABLING"

pattern StreamStatus_ENABLED :: StreamStatus
pattern StreamStatus_ENABLED = StreamStatus' "ENABLED"

pattern StreamStatus_ENABLING :: StreamStatus
pattern StreamStatus_ENABLING = StreamStatus' "ENABLING"

{-# COMPLETE
  StreamStatus_DISABLED,
  StreamStatus_DISABLING,
  StreamStatus_ENABLED,
  StreamStatus_ENABLING,
  StreamStatus'
  #-}
