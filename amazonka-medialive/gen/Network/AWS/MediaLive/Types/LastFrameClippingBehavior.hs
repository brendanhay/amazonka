{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.LastFrameClippingBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.LastFrameClippingBehavior
  ( LastFrameClippingBehavior
      ( ..,
        LastFrameClippingBehavior_EXCLUDE_LAST_FRAME,
        LastFrameClippingBehavior_INCLUDE_LAST_FRAME
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | If you specify a StopTimecode in an input (in order to clip the file),
-- you can specify if you want the clip to exclude (the default) or include
-- the frame specified by the timecode.
newtype LastFrameClippingBehavior = LastFrameClippingBehavior'
  { fromLastFrameClippingBehavior ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
