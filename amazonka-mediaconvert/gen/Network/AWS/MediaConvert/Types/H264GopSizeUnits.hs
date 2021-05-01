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
-- Module      : Network.AWS.MediaConvert.Types.H264GopSizeUnits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264GopSizeUnits
  ( H264GopSizeUnits
      ( ..,
        H264GopSizeUnits_FRAMES,
        H264GopSizeUnits_SECONDS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If
-- seconds the system will convert the GOP Size into a frame count at run
-- time.
newtype H264GopSizeUnits = H264GopSizeUnits'
  { fromH264GopSizeUnits ::
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

pattern H264GopSizeUnits_FRAMES :: H264GopSizeUnits
pattern H264GopSizeUnits_FRAMES = H264GopSizeUnits' "FRAMES"

pattern H264GopSizeUnits_SECONDS :: H264GopSizeUnits
pattern H264GopSizeUnits_SECONDS = H264GopSizeUnits' "SECONDS"

{-# COMPLETE
  H264GopSizeUnits_FRAMES,
  H264GopSizeUnits_SECONDS,
  H264GopSizeUnits'
  #-}
