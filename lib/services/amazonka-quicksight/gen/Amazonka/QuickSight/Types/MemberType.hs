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
-- Module      : Amazonka.QuickSight.Types.MemberType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MemberType
  ( MemberType
      ( ..,
        MemberType_ANALYSIS,
        MemberType_DASHBOARD,
        MemberType_DATASET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MemberType = MemberType'
  { fromMemberType ::
      Core.Text
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

pattern MemberType_ANALYSIS :: MemberType
pattern MemberType_ANALYSIS = MemberType' "ANALYSIS"

pattern MemberType_DASHBOARD :: MemberType
pattern MemberType_DASHBOARD = MemberType' "DASHBOARD"

pattern MemberType_DATASET :: MemberType
pattern MemberType_DATASET = MemberType' "DATASET"

{-# COMPLETE
  MemberType_ANALYSIS,
  MemberType_DASHBOARD,
  MemberType_DATASET,
  MemberType'
  #-}
