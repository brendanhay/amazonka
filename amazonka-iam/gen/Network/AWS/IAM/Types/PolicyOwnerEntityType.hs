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
-- Module      : Network.AWS.IAM.Types.PolicyOwnerEntityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyOwnerEntityType
  ( PolicyOwnerEntityType
      ( ..,
        PolicyOwnerEntityType_GROUP,
        PolicyOwnerEntityType_ROLE,
        PolicyOwnerEntityType_USER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PolicyOwnerEntityType = PolicyOwnerEntityType'
  { fromPolicyOwnerEntityType ::
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

pattern PolicyOwnerEntityType_GROUP :: PolicyOwnerEntityType
pattern PolicyOwnerEntityType_GROUP = PolicyOwnerEntityType' "GROUP"

pattern PolicyOwnerEntityType_ROLE :: PolicyOwnerEntityType
pattern PolicyOwnerEntityType_ROLE = PolicyOwnerEntityType' "ROLE"

pattern PolicyOwnerEntityType_USER :: PolicyOwnerEntityType
pattern PolicyOwnerEntityType_USER = PolicyOwnerEntityType' "USER"

{-# COMPLETE
  PolicyOwnerEntityType_GROUP,
  PolicyOwnerEntityType_ROLE,
  PolicyOwnerEntityType_USER,
  PolicyOwnerEntityType'
  #-}
