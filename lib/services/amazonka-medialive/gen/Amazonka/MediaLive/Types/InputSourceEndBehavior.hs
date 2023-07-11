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
-- Module      : Amazonka.MediaLive.Types.InputSourceEndBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSourceEndBehavior
  ( InputSourceEndBehavior
      ( ..,
        InputSourceEndBehavior_CONTINUE,
        InputSourceEndBehavior_LOOP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Input Source End Behavior
newtype InputSourceEndBehavior = InputSourceEndBehavior'
  { fromInputSourceEndBehavior ::
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

pattern InputSourceEndBehavior_CONTINUE :: InputSourceEndBehavior
pattern InputSourceEndBehavior_CONTINUE = InputSourceEndBehavior' "CONTINUE"

pattern InputSourceEndBehavior_LOOP :: InputSourceEndBehavior
pattern InputSourceEndBehavior_LOOP = InputSourceEndBehavior' "LOOP"

{-# COMPLETE
  InputSourceEndBehavior_CONTINUE,
  InputSourceEndBehavior_LOOP,
  InputSourceEndBehavior'
  #-}
