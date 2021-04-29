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

import qualified Network.AWS.Prelude as Prelude

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the
-- beginning of the archive as required for progressive downloading.
-- Otherwise it is placed normally at the end.
newtype Mp4MoovPlacement = Mp4MoovPlacement'
  { fromMp4MoovPlacement ::
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

pattern Mp4MoovPlacement_NORMAL :: Mp4MoovPlacement
pattern Mp4MoovPlacement_NORMAL = Mp4MoovPlacement' "NORMAL"

pattern Mp4MoovPlacement_PROGRESSIVE_DOWNLOAD :: Mp4MoovPlacement
pattern Mp4MoovPlacement_PROGRESSIVE_DOWNLOAD = Mp4MoovPlacement' "PROGRESSIVE_DOWNLOAD"

{-# COMPLETE
  Mp4MoovPlacement_NORMAL,
  Mp4MoovPlacement_PROGRESSIVE_DOWNLOAD,
  Mp4MoovPlacement'
  #-}
