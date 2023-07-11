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
-- Module      : Amazonka.RDS.Types.IAMAuthMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.IAMAuthMode
  ( IAMAuthMode
      ( ..,
        IAMAuthMode_DISABLED,
        IAMAuthMode_ENABLED,
        IAMAuthMode_REQUIRED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IAMAuthMode = IAMAuthMode'
  { fromIAMAuthMode ::
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

pattern IAMAuthMode_DISABLED :: IAMAuthMode
pattern IAMAuthMode_DISABLED = IAMAuthMode' "DISABLED"

pattern IAMAuthMode_ENABLED :: IAMAuthMode
pattern IAMAuthMode_ENABLED = IAMAuthMode' "ENABLED"

pattern IAMAuthMode_REQUIRED :: IAMAuthMode
pattern IAMAuthMode_REQUIRED = IAMAuthMode' "REQUIRED"

{-# COMPLETE
  IAMAuthMode_DISABLED,
  IAMAuthMode_ENABLED,
  IAMAuthMode_REQUIRED,
  IAMAuthMode'
  #-}
