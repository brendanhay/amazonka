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
-- Module      : Amazonka.Glue.Types.JoinType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JoinType
  ( JoinType
      ( ..,
        JoinType_Equijoin,
        JoinType_Left,
        JoinType_Leftanti,
        JoinType_Leftsemi,
        JoinType_Outer,
        JoinType_Right
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JoinType = JoinType'
  { fromJoinType ::
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

pattern JoinType_Equijoin :: JoinType
pattern JoinType_Equijoin = JoinType' "equijoin"

pattern JoinType_Left :: JoinType
pattern JoinType_Left = JoinType' "left"

pattern JoinType_Leftanti :: JoinType
pattern JoinType_Leftanti = JoinType' "leftanti"

pattern JoinType_Leftsemi :: JoinType
pattern JoinType_Leftsemi = JoinType' "leftsemi"

pattern JoinType_Outer :: JoinType
pattern JoinType_Outer = JoinType' "outer"

pattern JoinType_Right :: JoinType
pattern JoinType_Right = JoinType' "right"

{-# COMPLETE
  JoinType_Equijoin,
  JoinType_Left,
  JoinType_Leftanti,
  JoinType_Leftsemi,
  JoinType_Outer,
  JoinType_Right,
  JoinType'
  #-}
