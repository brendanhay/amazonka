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
-- Module      : Network.AWS.ResourceGroupsTagging.Types.TargetIdType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.TargetIdType
  ( TargetIdType
      ( ..,
        TargetIdType_ACCOUNT,
        TargetIdType_OU,
        TargetIdType_ROOT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TargetIdType = TargetIdType'
  { fromTargetIdType ::
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

pattern TargetIdType_ACCOUNT :: TargetIdType
pattern TargetIdType_ACCOUNT = TargetIdType' "ACCOUNT"

pattern TargetIdType_OU :: TargetIdType
pattern TargetIdType_OU = TargetIdType' "OU"

pattern TargetIdType_ROOT :: TargetIdType
pattern TargetIdType_ROOT = TargetIdType' "ROOT"

{-# COMPLETE
  TargetIdType_ACCOUNT,
  TargetIdType_OU,
  TargetIdType_ROOT,
  TargetIdType'
  #-}
