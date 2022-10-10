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
-- Module      : Amazonka.MediaConvert.Types.Eac3PassthroughControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3PassthroughControl
  ( Eac3PassthroughControl
      ( ..,
        Eac3PassthroughControl_NO_PASSTHROUGH,
        Eac3PassthroughControl_WHEN_POSSIBLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it
-- is present on the input. this detection is dynamic over the life of the
-- transcode. Inputs that alternate between DD+ and non-DD+ content will
-- have a consistent DD+ output as the system alternates between
-- passthrough and encoding.
newtype Eac3PassthroughControl = Eac3PassthroughControl'
  { fromEac3PassthroughControl ::
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

pattern Eac3PassthroughControl_NO_PASSTHROUGH :: Eac3PassthroughControl
pattern Eac3PassthroughControl_NO_PASSTHROUGH = Eac3PassthroughControl' "NO_PASSTHROUGH"

pattern Eac3PassthroughControl_WHEN_POSSIBLE :: Eac3PassthroughControl
pattern Eac3PassthroughControl_WHEN_POSSIBLE = Eac3PassthroughControl' "WHEN_POSSIBLE"

{-# COMPLETE
  Eac3PassthroughControl_NO_PASSTHROUGH,
  Eac3PassthroughControl_WHEN_POSSIBLE,
  Eac3PassthroughControl'
  #-}
