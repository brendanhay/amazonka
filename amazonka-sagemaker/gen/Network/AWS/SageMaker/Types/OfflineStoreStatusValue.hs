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
-- Module      : Network.AWS.SageMaker.Types.OfflineStoreStatusValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OfflineStoreStatusValue
  ( OfflineStoreStatusValue
      ( ..,
        OfflineStoreStatusValue_Active,
        OfflineStoreStatusValue_Blocked,
        OfflineStoreStatusValue_Disabled
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OfflineStoreStatusValue = OfflineStoreStatusValue'
  { fromOfflineStoreStatusValue ::
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

pattern OfflineStoreStatusValue_Active :: OfflineStoreStatusValue
pattern OfflineStoreStatusValue_Active = OfflineStoreStatusValue' "Active"

pattern OfflineStoreStatusValue_Blocked :: OfflineStoreStatusValue
pattern OfflineStoreStatusValue_Blocked = OfflineStoreStatusValue' "Blocked"

pattern OfflineStoreStatusValue_Disabled :: OfflineStoreStatusValue
pattern OfflineStoreStatusValue_Disabled = OfflineStoreStatusValue' "Disabled"

{-# COMPLETE
  OfflineStoreStatusValue_Active,
  OfflineStoreStatusValue_Blocked,
  OfflineStoreStatusValue_Disabled,
  OfflineStoreStatusValue'
  #-}
