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
-- Module      : Network.AWS.MediaPackage.Types.AdMarkers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.AdMarkers
  ( AdMarkers
      ( ..,
        AdMarkers_DATERANGE,
        AdMarkers_NONE,
        AdMarkers_PASSTHROUGH,
        AdMarkers_SCTE35_ENHANCED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AdMarkers = AdMarkers'
  { fromAdMarkers ::
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

pattern AdMarkers_DATERANGE :: AdMarkers
pattern AdMarkers_DATERANGE = AdMarkers' "DATERANGE"

pattern AdMarkers_NONE :: AdMarkers
pattern AdMarkers_NONE = AdMarkers' "NONE"

pattern AdMarkers_PASSTHROUGH :: AdMarkers
pattern AdMarkers_PASSTHROUGH = AdMarkers' "PASSTHROUGH"

pattern AdMarkers_SCTE35_ENHANCED :: AdMarkers
pattern AdMarkers_SCTE35_ENHANCED = AdMarkers' "SCTE35_ENHANCED"

{-# COMPLETE
  AdMarkers_DATERANGE,
  AdMarkers_NONE,
  AdMarkers_PASSTHROUGH,
  AdMarkers_SCTE35_ENHANCED,
  AdMarkers'
  #-}
