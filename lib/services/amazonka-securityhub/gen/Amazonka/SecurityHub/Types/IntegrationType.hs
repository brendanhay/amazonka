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
-- Module      : Amazonka.SecurityHub.Types.IntegrationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.IntegrationType
  ( IntegrationType
      ( ..,
        IntegrationType_RECEIVE_FINDINGS_FROM_SECURITY_HUB,
        IntegrationType_SEND_FINDINGS_TO_SECURITY_HUB,
        IntegrationType_UPDATE_FINDINGS_IN_SECURITY_HUB
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype IntegrationType = IntegrationType'
  { fromIntegrationType ::
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

pattern IntegrationType_RECEIVE_FINDINGS_FROM_SECURITY_HUB :: IntegrationType
pattern IntegrationType_RECEIVE_FINDINGS_FROM_SECURITY_HUB = IntegrationType' "RECEIVE_FINDINGS_FROM_SECURITY_HUB"

pattern IntegrationType_SEND_FINDINGS_TO_SECURITY_HUB :: IntegrationType
pattern IntegrationType_SEND_FINDINGS_TO_SECURITY_HUB = IntegrationType' "SEND_FINDINGS_TO_SECURITY_HUB"

pattern IntegrationType_UPDATE_FINDINGS_IN_SECURITY_HUB :: IntegrationType
pattern IntegrationType_UPDATE_FINDINGS_IN_SECURITY_HUB = IntegrationType' "UPDATE_FINDINGS_IN_SECURITY_HUB"

{-# COMPLETE
  IntegrationType_RECEIVE_FINDINGS_FROM_SECURITY_HUB,
  IntegrationType_SEND_FINDINGS_TO_SECURITY_HUB,
  IntegrationType_UPDATE_FINDINGS_IN_SECURITY_HUB,
  IntegrationType'
  #-}
