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
-- Module      : Network.AWS.Organizations.Types.TargetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.TargetType
  ( TargetType
      ( ..,
        TargetType_ACCOUNT,
        TargetType_ORGANIZATIONAL_UNIT,
        TargetType_ROOT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TargetType = TargetType'
  { fromTargetType ::
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

pattern TargetType_ACCOUNT :: TargetType
pattern TargetType_ACCOUNT = TargetType' "ACCOUNT"

pattern TargetType_ORGANIZATIONAL_UNIT :: TargetType
pattern TargetType_ORGANIZATIONAL_UNIT = TargetType' "ORGANIZATIONAL_UNIT"

pattern TargetType_ROOT :: TargetType
pattern TargetType_ROOT = TargetType' "ROOT"

{-# COMPLETE
  TargetType_ACCOUNT,
  TargetType_ORGANIZATIONAL_UNIT,
  TargetType_ROOT,
  TargetType'
  #-}
