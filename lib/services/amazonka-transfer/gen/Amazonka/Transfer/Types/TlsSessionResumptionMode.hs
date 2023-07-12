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
-- Module      : Amazonka.Transfer.Types.TlsSessionResumptionMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.TlsSessionResumptionMode
  ( TlsSessionResumptionMode
      ( ..,
        TlsSessionResumptionMode_DISABLED,
        TlsSessionResumptionMode_ENABLED,
        TlsSessionResumptionMode_ENFORCED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TlsSessionResumptionMode = TlsSessionResumptionMode'
  { fromTlsSessionResumptionMode ::
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

pattern TlsSessionResumptionMode_DISABLED :: TlsSessionResumptionMode
pattern TlsSessionResumptionMode_DISABLED = TlsSessionResumptionMode' "DISABLED"

pattern TlsSessionResumptionMode_ENABLED :: TlsSessionResumptionMode
pattern TlsSessionResumptionMode_ENABLED = TlsSessionResumptionMode' "ENABLED"

pattern TlsSessionResumptionMode_ENFORCED :: TlsSessionResumptionMode
pattern TlsSessionResumptionMode_ENFORCED = TlsSessionResumptionMode' "ENFORCED"

{-# COMPLETE
  TlsSessionResumptionMode_DISABLED,
  TlsSessionResumptionMode_ENABLED,
  TlsSessionResumptionMode_ENFORCED,
  TlsSessionResumptionMode'
  #-}
