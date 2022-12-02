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
-- Module      : Amazonka.DynamoDBStreams.Types.StreamStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types.StreamStatus
  ( StreamStatus
      ( ..,
        StreamStatus_DISABLED,
        StreamStatus_DISABLING,
        StreamStatus_ENABLED,
        StreamStatus_ENABLING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDBStreams.Internal
import qualified Amazonka.Prelude as Prelude

newtype StreamStatus = StreamStatus'
  { fromStreamStatus ::
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
