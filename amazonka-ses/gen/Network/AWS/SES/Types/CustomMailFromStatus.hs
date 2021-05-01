{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype CustomMailFromStatus = CustomMailFromStatus'
  { fromCustomMailFromStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
