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
-- Module      : Amazonka.MediaLive.Types.Fmp4NielsenId3Behavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Fmp4NielsenId3Behavior
  ( Fmp4NielsenId3Behavior
      ( ..,
        Fmp4NielsenId3Behavior_NO_PASSTHROUGH,
        Fmp4NielsenId3Behavior_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Fmp4 Nielsen Id3 Behavior
newtype Fmp4NielsenId3Behavior = Fmp4NielsenId3Behavior'
  { fromFmp4NielsenId3Behavior ::
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

pattern Fmp4NielsenId3Behavior_NO_PASSTHROUGH :: Fmp4NielsenId3Behavior
pattern Fmp4NielsenId3Behavior_NO_PASSTHROUGH = Fmp4NielsenId3Behavior' "NO_PASSTHROUGH"

pattern Fmp4NielsenId3Behavior_PASSTHROUGH :: Fmp4NielsenId3Behavior
pattern Fmp4NielsenId3Behavior_PASSTHROUGH = Fmp4NielsenId3Behavior' "PASSTHROUGH"

{-# COMPLETE
  Fmp4NielsenId3Behavior_NO_PASSTHROUGH,
  Fmp4NielsenId3Behavior_PASSTHROUGH,
  Fmp4NielsenId3Behavior'
  #-}
