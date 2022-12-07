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
-- Module      : Amazonka.QuickSight.Types.JoinType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.JoinType
  ( JoinType
      ( ..,
        JoinType_INNER,
        JoinType_LEFT,
        JoinType_OUTER,
        JoinType_RIGHT
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

pattern JoinType_INNER :: JoinType
pattern JoinType_INNER = JoinType' "INNER"

pattern JoinType_LEFT :: JoinType
pattern JoinType_LEFT = JoinType' "LEFT"

pattern JoinType_OUTER :: JoinType
pattern JoinType_OUTER = JoinType' "OUTER"

pattern JoinType_RIGHT :: JoinType
pattern JoinType_RIGHT = JoinType' "RIGHT"

{-# COMPLETE
  JoinType_INNER,
  JoinType_LEFT,
  JoinType_OUTER,
  JoinType_RIGHT,
  JoinType'
  #-}
