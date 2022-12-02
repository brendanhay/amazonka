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
-- Module      : Amazonka.MediaLive.Types.LastFrameClippingBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.LastFrameClippingBehavior
  ( LastFrameClippingBehavior
      ( ..,
        LastFrameClippingBehavior_EXCLUDE_LAST_FRAME,
        LastFrameClippingBehavior_INCLUDE_LAST_FRAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If you specify a StopTimecode in an input (in order to clip the file),
-- you can specify if you want the clip to exclude (the default) or include
-- the frame specified by the timecode.
newtype LastFrameClippingBehavior = LastFrameClippingBehavior'
  { fromLastFrameClippingBehavior ::
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

pattern LastFrameClippingBehavior_EXCLUDE_LAST_FRAME :: LastFrameClippingBehavior
pattern LastFrameClippingBehavior_EXCLUDE_LAST_FRAME = LastFrameClippingBehavior' "EXCLUDE_LAST_FRAME"

pattern LastFrameClippingBehavior_INCLUDE_LAST_FRAME :: LastFrameClippingBehavior
pattern LastFrameClippingBehavior_INCLUDE_LAST_FRAME = LastFrameClippingBehavior' "INCLUDE_LAST_FRAME"

{-# COMPLETE
  LastFrameClippingBehavior_EXCLUDE_LAST_FRAME,
  LastFrameClippingBehavior_INCLUDE_LAST_FRAME,
  LastFrameClippingBehavior'
  #-}
