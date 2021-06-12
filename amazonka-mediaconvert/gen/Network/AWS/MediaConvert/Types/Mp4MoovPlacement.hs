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
-- Module      : Network.AWS.MediaConvert.Types.Mp4MoovPlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4MoovPlacement
  ( Mp4MoovPlacement
      ( ..,
        Mp4MoovPlacement_NORMAL,
        Mp4MoovPlacement_PROGRESSIVE_DOWNLOAD
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
newtype Mp4MoovPlacement = Mp4MoovPlacement'
  { fromMp4MoovPlacement ::
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

pattern Mp4MoovPlacement_NORMAL :: Mp4MoovPlacement
pattern Mp4MoovPlacement_NORMAL = Mp4MoovPlacement' "NORMAL"

pattern Mp4MoovPlacement_PROGRESSIVE_DOWNLOAD :: Mp4MoovPlacement
pattern Mp4MoovPlacement_PROGRESSIVE_DOWNLOAD = Mp4MoovPlacement' "PROGRESSIVE_DOWNLOAD"

{-# COMPLETE
  Mp4MoovPlacement_NORMAL,
  Mp4MoovPlacement_PROGRESSIVE_DOWNLOAD,
  Mp4MoovPlacement'
  #-}
