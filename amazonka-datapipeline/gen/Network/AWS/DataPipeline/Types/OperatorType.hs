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
-- Module      : Network.AWS.DataPipeline.Types.OperatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.OperatorType
  ( OperatorType
      ( ..,
        OperatorType_BETWEEN,
        OperatorType_EQ,
        OperatorType_GE,
        OperatorType_LE,
        OperatorType_REF_EQ
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OperatorType = OperatorType'
  { fromOperatorType ::
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

pattern OperatorType_BETWEEN :: OperatorType
pattern OperatorType_BETWEEN = OperatorType' "BETWEEN"

pattern OperatorType_EQ :: OperatorType
pattern OperatorType_EQ = OperatorType' "EQ"

pattern OperatorType_GE :: OperatorType
pattern OperatorType_GE = OperatorType' "GE"

pattern OperatorType_LE :: OperatorType
pattern OperatorType_LE = OperatorType' "LE"

pattern OperatorType_REF_EQ :: OperatorType
pattern OperatorType_REF_EQ = OperatorType' "REF_EQ"

{-# COMPLETE
  OperatorType_BETWEEN,
  OperatorType_EQ,
  OperatorType_GE,
  OperatorType_LE,
  OperatorType_REF_EQ,
  OperatorType'
  #-}
