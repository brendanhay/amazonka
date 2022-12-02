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
-- Module      : Amazonka.MediaLive.Types.DvbSubOcrLanguage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSubOcrLanguage
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Dvb Sub Ocr Language
newtype DvbSubOcrLanguage = DvbSubOcrLanguage'
  { fromDvbSubOcrLanguage ::
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
