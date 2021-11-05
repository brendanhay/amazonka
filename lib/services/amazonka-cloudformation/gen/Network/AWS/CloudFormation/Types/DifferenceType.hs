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
-- Module      : Amazonka.CloudFormation.Types.DifferenceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.DifferenceType
  ( DifferenceType
      ( ..,
        DifferenceType_ADD,
        DifferenceType_NOT_EQUAL,
        DifferenceType_REMOVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DifferenceType = DifferenceType'
  { fromDifferenceType ::
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

pattern DifferenceType_ADD :: DifferenceType
pattern DifferenceType_ADD = DifferenceType' "ADD"

pattern DifferenceType_NOT_EQUAL :: DifferenceType
pattern DifferenceType_NOT_EQUAL = DifferenceType' "NOT_EQUAL"

pattern DifferenceType_REMOVE :: DifferenceType
pattern DifferenceType_REMOVE = DifferenceType' "REMOVE"

{-# COMPLETE
  DifferenceType_ADD,
  DifferenceType_NOT_EQUAL,
  DifferenceType_REMOVE,
  DifferenceType'
  #-}
