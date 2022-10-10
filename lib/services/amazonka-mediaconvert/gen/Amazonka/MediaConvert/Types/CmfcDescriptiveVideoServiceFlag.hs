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
-- Module      : Amazonka.MediaConvert.Types.CmfcDescriptiveVideoServiceFlag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmfcDescriptiveVideoServiceFlag
  ( CmfcDescriptiveVideoServiceFlag
      ( ..,
        CmfcDescriptiveVideoServiceFlag_DONT_FLAG,
        CmfcDescriptiveVideoServiceFlag_FLAG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Specify whether to flag this audio track as descriptive video service
-- (DVS) in your HLS parent manifest. When you choose Flag (FLAG),
-- MediaConvert includes the parameter
-- CHARACTERISTICS=\"public.accessibility.describes-video\" in the
-- EXT-X-MEDIA entry for this track. When you keep the default choice,
-- Don\'t flag (DONT_FLAG), MediaConvert leaves this parameter out. The DVS
-- flag can help with accessibility on Apple devices. For more information,
-- see the Apple documentation.
newtype CmfcDescriptiveVideoServiceFlag = CmfcDescriptiveVideoServiceFlag'
  { fromCmfcDescriptiveVideoServiceFlag ::
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

pattern CmfcDescriptiveVideoServiceFlag_DONT_FLAG :: CmfcDescriptiveVideoServiceFlag
pattern CmfcDescriptiveVideoServiceFlag_DONT_FLAG = CmfcDescriptiveVideoServiceFlag' "DONT_FLAG"

pattern CmfcDescriptiveVideoServiceFlag_FLAG :: CmfcDescriptiveVideoServiceFlag
pattern CmfcDescriptiveVideoServiceFlag_FLAG = CmfcDescriptiveVideoServiceFlag' "FLAG"

{-# COMPLETE
  CmfcDescriptiveVideoServiceFlag_DONT_FLAG,
  CmfcDescriptiveVideoServiceFlag_FLAG,
  CmfcDescriptiveVideoServiceFlag'
  #-}
