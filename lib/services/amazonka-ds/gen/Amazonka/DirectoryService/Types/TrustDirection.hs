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
-- Module      : Amazonka.DirectoryService.Types.TrustDirection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.TrustDirection
  ( TrustDirection
      ( ..,
        TrustDirection_One_Way__Incoming,
        TrustDirection_One_Way__Outgoing,
        TrustDirection_Two_Way
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TrustDirection = TrustDirection'
  { fromTrustDirection ::
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
