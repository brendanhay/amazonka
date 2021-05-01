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
-- Module      : Network.AWS.Organizations.Types.PolicyTypeStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTypeStatus
  ( PolicyTypeStatus
      ( ..,
        PolicyTypeStatus_ENABLED,
        PolicyTypeStatus_PENDING_DISABLE,
        PolicyTypeStatus_PENDING_ENABLE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PolicyTypeStatus = PolicyTypeStatus'
  { fromPolicyTypeStatus ::
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

pattern PolicyTypeStatus_ENABLED :: PolicyTypeStatus
pattern PolicyTypeStatus_ENABLED = PolicyTypeStatus' "ENABLED"

pattern PolicyTypeStatus_PENDING_DISABLE :: PolicyTypeStatus
pattern PolicyTypeStatus_PENDING_DISABLE = PolicyTypeStatus' "PENDING_DISABLE"

pattern PolicyTypeStatus_PENDING_ENABLE :: PolicyTypeStatus
pattern PolicyTypeStatus_PENDING_ENABLE = PolicyTypeStatus' "PENDING_ENABLE"

{-# COMPLETE
  PolicyTypeStatus_ENABLED,
  PolicyTypeStatus_PENDING_DISABLE,
  PolicyTypeStatus_PENDING_ENABLE,
  PolicyTypeStatus'
  #-}
