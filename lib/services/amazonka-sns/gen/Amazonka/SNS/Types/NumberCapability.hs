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
-- Module      : Amazonka.SNS.Types.NumberCapability
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.NumberCapability
  ( NumberCapability
      ( ..,
        NumberCapability_MMS,
        NumberCapability_SMS,
        NumberCapability_VOICE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Enum listing out all supported number capabilities.
newtype NumberCapability = NumberCapability'
  { fromNumberCapability ::
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

pattern NumberCapability_MMS :: NumberCapability
pattern NumberCapability_MMS = NumberCapability' "MMS"

pattern NumberCapability_SMS :: NumberCapability
pattern NumberCapability_SMS = NumberCapability' "SMS"

pattern NumberCapability_VOICE :: NumberCapability
pattern NumberCapability_VOICE = NumberCapability' "VOICE"

{-# COMPLETE
  NumberCapability_MMS,
  NumberCapability_SMS,
  NumberCapability_VOICE,
  NumberCapability'
  #-}
