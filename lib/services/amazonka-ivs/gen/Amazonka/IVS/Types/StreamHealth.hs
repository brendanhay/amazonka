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
-- Module      : Amazonka.IVS.Types.StreamHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.StreamHealth
  ( StreamHealth
      ( ..,
        StreamHealth_HEALTHY,
        StreamHealth_STARVING,
        StreamHealth_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StreamHealth = StreamHealth'
  { fromStreamHealth ::
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

pattern StreamHealth_HEALTHY :: StreamHealth
pattern StreamHealth_HEALTHY = StreamHealth' "HEALTHY"

pattern StreamHealth_STARVING :: StreamHealth
pattern StreamHealth_STARVING = StreamHealth' "STARVING"

pattern StreamHealth_UNKNOWN :: StreamHealth
pattern StreamHealth_UNKNOWN = StreamHealth' "UNKNOWN"

{-# COMPLETE
  StreamHealth_HEALTHY,
  StreamHealth_STARVING,
  StreamHealth_UNKNOWN,
  StreamHealth'
  #-}
