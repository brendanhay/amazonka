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
-- Module      : Amazonka.CodeCommit.Types.ChangeTypeEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ChangeTypeEnum
  ( ChangeTypeEnum
      ( ..,
        ChangeTypeEnum_A,
        ChangeTypeEnum_D,
        ChangeTypeEnum_M
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ChangeTypeEnum = ChangeTypeEnum'
  { fromChangeTypeEnum ::
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

pattern ChangeTypeEnum_A :: ChangeTypeEnum
pattern ChangeTypeEnum_A = ChangeTypeEnum' "A"

pattern ChangeTypeEnum_D :: ChangeTypeEnum
pattern ChangeTypeEnum_D = ChangeTypeEnum' "D"

pattern ChangeTypeEnum_M :: ChangeTypeEnum
pattern ChangeTypeEnum_M = ChangeTypeEnum' "M"

{-# COMPLETE
  ChangeTypeEnum_A,
  ChangeTypeEnum_D,
  ChangeTypeEnum_M,
  ChangeTypeEnum'
  #-}
