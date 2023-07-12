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
-- Module      : Amazonka.CloudWatch.Types.StatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.StatusCode
  ( StatusCode
      ( ..,
        StatusCode_Complete,
        StatusCode_Forbidden,
        StatusCode_InternalError,
        StatusCode_PartialData
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StatusCode = StatusCode'
  { fromStatusCode ::
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

pattern StatusCode_Complete :: StatusCode
pattern StatusCode_Complete = StatusCode' "Complete"

pattern StatusCode_Forbidden :: StatusCode
pattern StatusCode_Forbidden = StatusCode' "Forbidden"

pattern StatusCode_InternalError :: StatusCode
pattern StatusCode_InternalError = StatusCode' "InternalError"

pattern StatusCode_PartialData :: StatusCode
pattern StatusCode_PartialData = StatusCode' "PartialData"

{-# COMPLETE
  StatusCode_Complete,
  StatusCode_Forbidden,
  StatusCode_InternalError,
  StatusCode_PartialData,
  StatusCode'
  #-}
