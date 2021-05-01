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

import qualified Network.AWS.Prelude as Prelude

newtype OperatorType = OperatorType'
  { fromOperatorType ::
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
