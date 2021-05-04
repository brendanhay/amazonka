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
-- Module      : Network.AWS.Lightsail.Types.RenewalStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RenewalStatus
  ( RenewalStatus
      ( ..,
        RenewalStatus_Failed,
        RenewalStatus_PendingAutoRenewal,
        RenewalStatus_PendingValidation,
        RenewalStatus_Success
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RenewalStatus = RenewalStatus'
  { fromRenewalStatus ::
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

pattern RenewalStatus_Failed :: RenewalStatus
pattern RenewalStatus_Failed = RenewalStatus' "Failed"

pattern RenewalStatus_PendingAutoRenewal :: RenewalStatus
pattern RenewalStatus_PendingAutoRenewal = RenewalStatus' "PendingAutoRenewal"

pattern RenewalStatus_PendingValidation :: RenewalStatus
pattern RenewalStatus_PendingValidation = RenewalStatus' "PendingValidation"

pattern RenewalStatus_Success :: RenewalStatus
pattern RenewalStatus_Success = RenewalStatus' "Success"

{-# COMPLETE
  RenewalStatus_Failed,
  RenewalStatus_PendingAutoRenewal,
  RenewalStatus_PendingValidation,
  RenewalStatus_Success,
  RenewalStatus'
  #-}
