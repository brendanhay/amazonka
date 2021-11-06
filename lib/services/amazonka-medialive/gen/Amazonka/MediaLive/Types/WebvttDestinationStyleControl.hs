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
-- Module      : Amazonka.MediaLive.Types.WebvttDestinationStyleControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.WebvttDestinationStyleControl
  ( WebvttDestinationStyleControl
      ( ..,
        WebvttDestinationStyleControl_NO_STYLE_DATA,
        WebvttDestinationStyleControl_PASSTHROUGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Webvtt Destination Style Control
newtype WebvttDestinationStyleControl = WebvttDestinationStyleControl'
  { fromWebvttDestinationStyleControl ::
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

pattern WebvttDestinationStyleControl_NO_STYLE_DATA :: WebvttDestinationStyleControl
pattern WebvttDestinationStyleControl_NO_STYLE_DATA = WebvttDestinationStyleControl' "NO_STYLE_DATA"

pattern WebvttDestinationStyleControl_PASSTHROUGH :: WebvttDestinationStyleControl
pattern WebvttDestinationStyleControl_PASSTHROUGH = WebvttDestinationStyleControl' "PASSTHROUGH"

{-# COMPLETE
  WebvttDestinationStyleControl_NO_STYLE_DATA,
  WebvttDestinationStyleControl_PASSTHROUGH,
  WebvttDestinationStyleControl'
  #-}
