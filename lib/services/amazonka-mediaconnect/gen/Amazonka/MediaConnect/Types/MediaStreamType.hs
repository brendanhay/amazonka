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
-- Module      : Amazonka.MediaConnect.Types.MediaStreamType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.MediaStreamType
  ( MediaStreamType
      ( ..,
        MediaStreamType_Ancillary_data,
        MediaStreamType_Audio,
        MediaStreamType_Video
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MediaStreamType = MediaStreamType'
  { fromMediaStreamType ::
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

pattern MediaStreamType_Ancillary_data :: MediaStreamType
pattern MediaStreamType_Ancillary_data = MediaStreamType' "ancillary-data"

pattern MediaStreamType_Audio :: MediaStreamType
pattern MediaStreamType_Audio = MediaStreamType' "audio"

pattern MediaStreamType_Video :: MediaStreamType
pattern MediaStreamType_Video = MediaStreamType' "video"

{-# COMPLETE
  MediaStreamType_Ancillary_data,
  MediaStreamType_Audio,
  MediaStreamType_Video,
  MediaStreamType'
  #-}
