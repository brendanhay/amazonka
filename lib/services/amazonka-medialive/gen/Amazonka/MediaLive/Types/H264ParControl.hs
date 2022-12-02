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
-- Module      : Amazonka.MediaLive.Types.H264ParControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264ParControl
  ( H264ParControl
      ( ..,
        H264ParControl_INITIALIZE_FROM_SOURCE,
        H264ParControl_SPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H264 Par Control
newtype H264ParControl = H264ParControl'
  { fromH264ParControl ::
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

pattern H264ParControl_INITIALIZE_FROM_SOURCE :: H264ParControl
pattern H264ParControl_INITIALIZE_FROM_SOURCE = H264ParControl' "INITIALIZE_FROM_SOURCE"

pattern H264ParControl_SPECIFIED :: H264ParControl
pattern H264ParControl_SPECIFIED = H264ParControl' "SPECIFIED"

{-# COMPLETE
  H264ParControl_INITIALIZE_FROM_SOURCE,
  H264ParControl_SPECIFIED,
  H264ParControl'
  #-}
