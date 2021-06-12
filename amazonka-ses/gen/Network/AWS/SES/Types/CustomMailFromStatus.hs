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
-- Module      : Network.AWS.SES.Types.CustomMailFromStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CustomMailFromStatus
  ( CustomMailFromStatus
      ( ..,
        CustomMailFromStatus_Failed,
        CustomMailFromStatus_Pending,
        CustomMailFromStatus_Success,
        CustomMailFromStatus_TemporaryFailure
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CustomMailFromStatus = CustomMailFromStatus'
  { fromCustomMailFromStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
