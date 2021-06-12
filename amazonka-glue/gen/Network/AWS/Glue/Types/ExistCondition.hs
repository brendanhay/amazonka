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
-- Module      : Network.AWS.Glue.Types.ExistCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ExistCondition
  ( ExistCondition
      ( ..,
        ExistCondition_MUST_EXIST,
        ExistCondition_NONE,
        ExistCondition_NOT_EXIST
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ExistCondition = ExistCondition'
  { fromExistCondition ::
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

pattern ExistCondition_MUST_EXIST :: ExistCondition
pattern ExistCondition_MUST_EXIST = ExistCondition' "MUST_EXIST"

pattern ExistCondition_NONE :: ExistCondition
pattern ExistCondition_NONE = ExistCondition' "NONE"

pattern ExistCondition_NOT_EXIST :: ExistCondition
pattern ExistCondition_NOT_EXIST = ExistCondition' "NOT_EXIST"

{-# COMPLETE
  ExistCondition_MUST_EXIST,
  ExistCondition_NONE,
  ExistCondition_NOT_EXIST,
  ExistCondition'
  #-}
