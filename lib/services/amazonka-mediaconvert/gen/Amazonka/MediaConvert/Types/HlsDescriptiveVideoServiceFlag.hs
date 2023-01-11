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
-- Module      : Amazonka.MediaConvert.Types.HlsDescriptiveVideoServiceFlag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsDescriptiveVideoServiceFlag
  ( HlsDescriptiveVideoServiceFlag
      ( ..,
        HlsDescriptiveVideoServiceFlag_DONT_FLAG,
        HlsDescriptiveVideoServiceFlag_FLAG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether to flag this audio track as descriptive video service
-- (DVS) in your HLS parent manifest. When you choose Flag (FLAG),
-- MediaConvert includes the parameter
-- CHARACTERISTICS=\"public.accessibility.describes-video\" in the
-- EXT-X-MEDIA entry for this track. When you keep the default choice,
-- Don\'t flag (DONT_FLAG), MediaConvert leaves this parameter out. The DVS
-- flag can help with accessibility on Apple devices. For more information,
-- see the Apple documentation.
newtype HlsDescriptiveVideoServiceFlag = HlsDescriptiveVideoServiceFlag'
  { fromHlsDescriptiveVideoServiceFlag ::
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

pattern HlsDescriptiveVideoServiceFlag_DONT_FLAG :: HlsDescriptiveVideoServiceFlag
pattern HlsDescriptiveVideoServiceFlag_DONT_FLAG = HlsDescriptiveVideoServiceFlag' "DONT_FLAG"

pattern HlsDescriptiveVideoServiceFlag_FLAG :: HlsDescriptiveVideoServiceFlag
pattern HlsDescriptiveVideoServiceFlag_FLAG = HlsDescriptiveVideoServiceFlag' "FLAG"

{-# COMPLETE
  HlsDescriptiveVideoServiceFlag_DONT_FLAG,
  HlsDescriptiveVideoServiceFlag_FLAG,
  HlsDescriptiveVideoServiceFlag'
  #-}
