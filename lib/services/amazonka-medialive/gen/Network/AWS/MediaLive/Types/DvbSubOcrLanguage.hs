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
-- Module      : Network.AWS.MediaLive.Types.DvbSubOcrLanguage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubOcrLanguage
  ( DvbSubOcrLanguage
      ( ..,
        DvbSubOcrLanguage_DEU,
        DvbSubOcrLanguage_ENG,
        DvbSubOcrLanguage_FRA,
        DvbSubOcrLanguage_NLD,
        DvbSubOcrLanguage_POR,
        DvbSubOcrLanguage_SPA
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Dvb Sub Ocr Language
newtype DvbSubOcrLanguage = DvbSubOcrLanguage'
  { fromDvbSubOcrLanguage ::
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

pattern DvbSubOcrLanguage_DEU :: DvbSubOcrLanguage
pattern DvbSubOcrLanguage_DEU = DvbSubOcrLanguage' "DEU"

pattern DvbSubOcrLanguage_ENG :: DvbSubOcrLanguage
pattern DvbSubOcrLanguage_ENG = DvbSubOcrLanguage' "ENG"

pattern DvbSubOcrLanguage_FRA :: DvbSubOcrLanguage
pattern DvbSubOcrLanguage_FRA = DvbSubOcrLanguage' "FRA"

pattern DvbSubOcrLanguage_NLD :: DvbSubOcrLanguage
pattern DvbSubOcrLanguage_NLD = DvbSubOcrLanguage' "NLD"

pattern DvbSubOcrLanguage_POR :: DvbSubOcrLanguage
pattern DvbSubOcrLanguage_POR = DvbSubOcrLanguage' "POR"

pattern DvbSubOcrLanguage_SPA :: DvbSubOcrLanguage
pattern DvbSubOcrLanguage_SPA = DvbSubOcrLanguage' "SPA"

{-# COMPLETE
  DvbSubOcrLanguage_DEU,
  DvbSubOcrLanguage_ENG,
  DvbSubOcrLanguage_FRA,
  DvbSubOcrLanguage_NLD,
  DvbSubOcrLanguage_POR,
  DvbSubOcrLanguage_SPA,
  DvbSubOcrLanguage'
  #-}
