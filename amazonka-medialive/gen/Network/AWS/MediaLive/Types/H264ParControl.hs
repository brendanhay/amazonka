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
-- Module      : Network.AWS.MediaLive.Types.H264ParControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ParControl
  ( H264ParControl
      ( ..,
        H264ParControl_INITIALIZE_FROM_SOURCE,
        H264ParControl_SPECIFIED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | H264 Par Control
newtype H264ParControl = H264ParControl'
  { fromH264ParControl ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern H264ParControl_INITIALIZE_FROM_SOURCE :: H264ParControl
pattern H264ParControl_INITIALIZE_FROM_SOURCE = H264ParControl' "INITIALIZE_FROM_SOURCE"

pattern H264ParControl_SPECIFIED :: H264ParControl
pattern H264ParControl_SPECIFIED = H264ParControl' "SPECIFIED"

{-# COMPLETE
  H264ParControl_INITIALIZE_FROM_SOURCE,
  H264ParControl_SPECIFIED,
  H264ParControl'
  #-}
