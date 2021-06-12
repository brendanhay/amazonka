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
-- Module      : Network.AWS.DirectoryService.Types.TrustDirection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TrustDirection
  ( TrustDirection
      ( ..,
        TrustDirection_One_Way__Incoming,
        TrustDirection_One_Way__Outgoing,
        TrustDirection_Two_Way
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TrustDirection = TrustDirection'
  { fromTrustDirection ::
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

pattern TrustDirection_One_Way__Incoming :: TrustDirection
pattern TrustDirection_One_Way__Incoming = TrustDirection' "One-Way: Incoming"

pattern TrustDirection_One_Way__Outgoing :: TrustDirection
pattern TrustDirection_One_Way__Outgoing = TrustDirection' "One-Way: Outgoing"

pattern TrustDirection_Two_Way :: TrustDirection
pattern TrustDirection_Two_Way = TrustDirection' "Two-Way"

{-# COMPLETE
  TrustDirection_One_Way__Incoming,
  TrustDirection_One_Way__Outgoing,
  TrustDirection_Two_Way,
  TrustDirection'
  #-}
