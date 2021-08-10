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
-- Module      : Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TtmlDestinationStyleControl
  ( TtmlDestinationStyleControl
      ( ..,
        TtmlDestinationStyleControl_PASSTHROUGH,
        TtmlDestinationStyleControl_USE_CONFIGURED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Ttml Destination Style Control
newtype TtmlDestinationStyleControl = TtmlDestinationStyleControl'
  { fromTtmlDestinationStyleControl ::
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

pattern TtmlDestinationStyleControl_PASSTHROUGH :: TtmlDestinationStyleControl
pattern TtmlDestinationStyleControl_PASSTHROUGH = TtmlDestinationStyleControl' "PASSTHROUGH"

pattern TtmlDestinationStyleControl_USE_CONFIGURED :: TtmlDestinationStyleControl
pattern TtmlDestinationStyleControl_USE_CONFIGURED = TtmlDestinationStyleControl' "USE_CONFIGURED"

{-# COMPLETE
  TtmlDestinationStyleControl_PASSTHROUGH,
  TtmlDestinationStyleControl_USE_CONFIGURED,
  TtmlDestinationStyleControl'
  #-}
