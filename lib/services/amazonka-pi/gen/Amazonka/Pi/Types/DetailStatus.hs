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
-- Module      : Amazonka.Pi.Types.DetailStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.DetailStatus
  ( DetailStatus
      ( ..,
        DetailStatus_AVAILABLE,
        DetailStatus_PROCESSING,
        DetailStatus_UNAVAILABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DetailStatus = DetailStatus'
  { fromDetailStatus ::
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

pattern DetailStatus_AVAILABLE :: DetailStatus
pattern DetailStatus_AVAILABLE = DetailStatus' "AVAILABLE"

pattern DetailStatus_PROCESSING :: DetailStatus
pattern DetailStatus_PROCESSING = DetailStatus' "PROCESSING"

pattern DetailStatus_UNAVAILABLE :: DetailStatus
pattern DetailStatus_UNAVAILABLE = DetailStatus' "UNAVAILABLE"

{-# COMPLETE
  DetailStatus_AVAILABLE,
  DetailStatus_PROCESSING,
  DetailStatus_UNAVAILABLE,
  DetailStatus'
  #-}
