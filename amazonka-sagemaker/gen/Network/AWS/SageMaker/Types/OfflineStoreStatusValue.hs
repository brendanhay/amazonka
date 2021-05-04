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

import qualified Network.AWS.Prelude as Prelude

newtype OfflineStoreStatusValue = OfflineStoreStatusValue'
  { fromOfflineStoreStatusValue ::
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
