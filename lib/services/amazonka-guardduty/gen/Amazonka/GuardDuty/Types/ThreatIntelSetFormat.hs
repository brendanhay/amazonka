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
-- Module      : Amazonka.GuardDuty.Types.ThreatIntelSetFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ThreatIntelSetFormat
  ( ThreatIntelSetFormat
      ( ..,
        ThreatIntelSetFormat_ALIEN_VAULT,
        ThreatIntelSetFormat_FIRE_EYE,
        ThreatIntelSetFormat_OTX_CSV,
        ThreatIntelSetFormat_PROOF_POINT,
        ThreatIntelSetFormat_STIX,
        ThreatIntelSetFormat_TXT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ThreatIntelSetFormat = ThreatIntelSetFormat'
  { fromThreatIntelSetFormat ::
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

pattern ThreatIntelSetFormat_ALIEN_VAULT :: ThreatIntelSetFormat
pattern ThreatIntelSetFormat_ALIEN_VAULT = ThreatIntelSetFormat' "ALIEN_VAULT"

pattern ThreatIntelSetFormat_FIRE_EYE :: ThreatIntelSetFormat
pattern ThreatIntelSetFormat_FIRE_EYE = ThreatIntelSetFormat' "FIRE_EYE"

pattern ThreatIntelSetFormat_OTX_CSV :: ThreatIntelSetFormat
pattern ThreatIntelSetFormat_OTX_CSV = ThreatIntelSetFormat' "OTX_CSV"

pattern ThreatIntelSetFormat_PROOF_POINT :: ThreatIntelSetFormat
pattern ThreatIntelSetFormat_PROOF_POINT = ThreatIntelSetFormat' "PROOF_POINT"

pattern ThreatIntelSetFormat_STIX :: ThreatIntelSetFormat
pattern ThreatIntelSetFormat_STIX = ThreatIntelSetFormat' "STIX"

pattern ThreatIntelSetFormat_TXT :: ThreatIntelSetFormat
pattern ThreatIntelSetFormat_TXT = ThreatIntelSetFormat' "TXT"

{-# COMPLETE
  ThreatIntelSetFormat_ALIEN_VAULT,
  ThreatIntelSetFormat_FIRE_EYE,
  ThreatIntelSetFormat_OTX_CSV,
  ThreatIntelSetFormat_PROOF_POINT,
  ThreatIntelSetFormat_STIX,
  ThreatIntelSetFormat_TXT,
  ThreatIntelSetFormat'
  #-}
