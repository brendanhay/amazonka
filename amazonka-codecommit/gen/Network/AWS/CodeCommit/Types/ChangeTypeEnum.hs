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
-- Module      : Network.AWS.CodeCommit.Types.ChangeTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ChangeTypeEnum
  ( ChangeTypeEnum
      ( ..,
        ChangeTypeEnum_A,
        ChangeTypeEnum_D,
        ChangeTypeEnum_M
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ChangeTypeEnum = ChangeTypeEnum'
  { fromChangeTypeEnum ::
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
