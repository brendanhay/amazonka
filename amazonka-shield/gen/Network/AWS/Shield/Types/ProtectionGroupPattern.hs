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
-- Module      : Network.AWS.Shield.Types.ProtectionGroupPattern
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupPattern
  ( ProtectionGroupPattern
      ( ..,
        ProtectionGroupPattern_ALL,
        ProtectionGroupPattern_ARBITRARY,
        ProtectionGroupPattern_BY_RESOURCE_TYPE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ProtectionGroupPattern = ProtectionGroupPattern'
  { fromProtectionGroupPattern ::
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

pattern ProtectionGroupPattern_ALL :: ProtectionGroupPattern
pattern ProtectionGroupPattern_ALL = ProtectionGroupPattern' "ALL"

pattern ProtectionGroupPattern_ARBITRARY :: ProtectionGroupPattern
pattern ProtectionGroupPattern_ARBITRARY = ProtectionGroupPattern' "ARBITRARY"

pattern ProtectionGroupPattern_BY_RESOURCE_TYPE :: ProtectionGroupPattern
pattern ProtectionGroupPattern_BY_RESOURCE_TYPE = ProtectionGroupPattern' "BY_RESOURCE_TYPE"

{-# COMPLETE
  ProtectionGroupPattern_ALL,
  ProtectionGroupPattern_ARBITRARY,
  ProtectionGroupPattern_BY_RESOURCE_TYPE,
  ProtectionGroupPattern'
  #-}
