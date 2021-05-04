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
-- Module      : Network.AWS.CloudFormation.Types.DifferenceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.DifferenceType
  ( DifferenceType
      ( ..,
        DifferenceType_ADD,
        DifferenceType_NOT_EQUAL,
        DifferenceType_REMOVE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DifferenceType = DifferenceType'
  { fromDifferenceType ::
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
