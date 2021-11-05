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
-- Module      : Network.AWS.SecurityHub.Types.SeverityLabel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.SeverityLabel
  ( SeverityLabel
      ( ..,
        SeverityLabel_CRITICAL,
        SeverityLabel_HIGH,
        SeverityLabel_INFORMATIONAL,
        SeverityLabel_LOW,
        SeverityLabel_MEDIUM
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SeverityLabel = SeverityLabel'
  { fromSeverityLabel ::
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

pattern SeverityLabel_CRITICAL :: SeverityLabel
pattern SeverityLabel_CRITICAL = SeverityLabel' "CRITICAL"

pattern SeverityLabel_HIGH :: SeverityLabel
pattern SeverityLabel_HIGH = SeverityLabel' "HIGH"

pattern SeverityLabel_INFORMATIONAL :: SeverityLabel
pattern SeverityLabel_INFORMATIONAL = SeverityLabel' "INFORMATIONAL"

pattern SeverityLabel_LOW :: SeverityLabel
pattern SeverityLabel_LOW = SeverityLabel' "LOW"

pattern SeverityLabel_MEDIUM :: SeverityLabel
pattern SeverityLabel_MEDIUM = SeverityLabel' "MEDIUM"

{-# COMPLETE
  SeverityLabel_CRITICAL,
  SeverityLabel_HIGH,
  SeverityLabel_INFORMATIONAL,
  SeverityLabel_LOW,
  SeverityLabel_MEDIUM,
  SeverityLabel'
  #-}
