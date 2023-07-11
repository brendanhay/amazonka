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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Webvtt Destination Style Control
newtype WebvttDestinationStyleControl = WebvttDestinationStyleControl'
  { fromWebvttDestinationStyleControl ::
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

pattern WebvttDestinationStyleControl_NO_STYLE_DATA :: WebvttDestinationStyleControl
pattern WebvttDestinationStyleControl_NO_STYLE_DATA = WebvttDestinationStyleControl' "NO_STYLE_DATA"

pattern WebvttDestinationStyleControl_PASSTHROUGH :: WebvttDestinationStyleControl
pattern WebvttDestinationStyleControl_PASSTHROUGH = WebvttDestinationStyleControl' "PASSTHROUGH"

{-# COMPLETE
  WebvttDestinationStyleControl_NO_STYLE_DATA,
  WebvttDestinationStyleControl_PASSTHROUGH,
  WebvttDestinationStyleControl'
  #-}
