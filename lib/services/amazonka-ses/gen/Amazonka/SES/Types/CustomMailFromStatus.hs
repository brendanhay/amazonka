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
-- Module      : Amazonka.SES.Types.CustomMailFromStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.CustomMailFromStatus
  ( CustomMailFromStatus
      ( ..,
        CustomMailFromStatus_Failed,
        CustomMailFromStatus_Pending,
        CustomMailFromStatus_Success,
        CustomMailFromStatus_TemporaryFailure
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomMailFromStatus = CustomMailFromStatus'
  { fromCustomMailFromStatus ::
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

pattern CustomMailFromStatus_Failed :: CustomMailFromStatus
pattern CustomMailFromStatus_Failed = CustomMailFromStatus' "Failed"

pattern CustomMailFromStatus_Pending :: CustomMailFromStatus
pattern CustomMailFromStatus_Pending = CustomMailFromStatus' "Pending"

pattern CustomMailFromStatus_Success :: CustomMailFromStatus
pattern CustomMailFromStatus_Success = CustomMailFromStatus' "Success"

pattern CustomMailFromStatus_TemporaryFailure :: CustomMailFromStatus
pattern CustomMailFromStatus_TemporaryFailure = CustomMailFromStatus' "TemporaryFailure"

{-# COMPLETE
  CustomMailFromStatus_Failed,
  CustomMailFromStatus_Pending,
  CustomMailFromStatus_Success,
  CustomMailFromStatus_TemporaryFailure,
  CustomMailFromStatus'
  #-}
