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
-- Module      : Amazonka.MediaLive.Types.Eac3SurroundExMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3SurroundExMode
  ( Eac3SurroundExMode
      ( ..,
        Eac3SurroundExMode_DISABLED,
        Eac3SurroundExMode_ENABLED,
        Eac3SurroundExMode_NOT_INDICATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Surround Ex Mode
newtype Eac3SurroundExMode = Eac3SurroundExMode'
  { fromEac3SurroundExMode ::
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

pattern Eac3SurroundExMode_DISABLED :: Eac3SurroundExMode
pattern Eac3SurroundExMode_DISABLED = Eac3SurroundExMode' "DISABLED"

pattern Eac3SurroundExMode_ENABLED :: Eac3SurroundExMode
pattern Eac3SurroundExMode_ENABLED = Eac3SurroundExMode' "ENABLED"

pattern Eac3SurroundExMode_NOT_INDICATED :: Eac3SurroundExMode
pattern Eac3SurroundExMode_NOT_INDICATED = Eac3SurroundExMode' "NOT_INDICATED"

{-# COMPLETE
  Eac3SurroundExMode_DISABLED,
  Eac3SurroundExMode_ENABLED,
  Eac3SurroundExMode_NOT_INDICATED,
  Eac3SurroundExMode'
  #-}
