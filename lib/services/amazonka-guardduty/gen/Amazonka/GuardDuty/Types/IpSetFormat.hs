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
-- Module      : Amazonka.GuardDuty.Types.IpSetFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.IpSetFormat
  ( IpSetFormat
      ( ..,
        IpSetFormat_ALIEN_VAULT,
        IpSetFormat_FIRE_EYE,
        IpSetFormat_OTX_CSV,
        IpSetFormat_PROOF_POINT,
        IpSetFormat_STIX,
        IpSetFormat_TXT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IpSetFormat = IpSetFormat'
  { fromIpSetFormat ::
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

pattern IpSetFormat_ALIEN_VAULT :: IpSetFormat
pattern IpSetFormat_ALIEN_VAULT = IpSetFormat' "ALIEN_VAULT"

pattern IpSetFormat_FIRE_EYE :: IpSetFormat
pattern IpSetFormat_FIRE_EYE = IpSetFormat' "FIRE_EYE"

pattern IpSetFormat_OTX_CSV :: IpSetFormat
pattern IpSetFormat_OTX_CSV = IpSetFormat' "OTX_CSV"

pattern IpSetFormat_PROOF_POINT :: IpSetFormat
pattern IpSetFormat_PROOF_POINT = IpSetFormat' "PROOF_POINT"

pattern IpSetFormat_STIX :: IpSetFormat
pattern IpSetFormat_STIX = IpSetFormat' "STIX"

pattern IpSetFormat_TXT :: IpSetFormat
pattern IpSetFormat_TXT = IpSetFormat' "TXT"

{-# COMPLETE
  IpSetFormat_ALIEN_VAULT,
  IpSetFormat_FIRE_EYE,
  IpSetFormat_OTX_CSV,
  IpSetFormat_PROOF_POINT,
  IpSetFormat_STIX,
  IpSetFormat_TXT,
  IpSetFormat'
  #-}
