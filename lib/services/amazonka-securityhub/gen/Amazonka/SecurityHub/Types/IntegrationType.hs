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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IntegrationType = IntegrationType'
  { fromIntegrationType ::
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
