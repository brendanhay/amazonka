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

import qualified Network.AWS.Prelude as Prelude

newtype TrustDirection = TrustDirection'
  { fromTrustDirection ::
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
