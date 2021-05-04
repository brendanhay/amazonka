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
-- Module      : Network.AWS.DirectConnect.Types.HasLogicalRedundancy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.HasLogicalRedundancy
  ( HasLogicalRedundancy
      ( ..,
        HasLogicalRedundancy_No,
        HasLogicalRedundancy_Unknown,
        HasLogicalRedundancy_Yes
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype HasLogicalRedundancy = HasLogicalRedundancy'
  { fromHasLogicalRedundancy ::
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

pattern HasLogicalRedundancy_No :: HasLogicalRedundancy
pattern HasLogicalRedundancy_No = HasLogicalRedundancy' "no"

pattern HasLogicalRedundancy_Unknown :: HasLogicalRedundancy
pattern HasLogicalRedundancy_Unknown = HasLogicalRedundancy' "unknown"

pattern HasLogicalRedundancy_Yes :: HasLogicalRedundancy
pattern HasLogicalRedundancy_Yes = HasLogicalRedundancy' "yes"

{-# COMPLETE
  HasLogicalRedundancy_No,
  HasLogicalRedundancy_Unknown,
  HasLogicalRedundancy_Yes,
  HasLogicalRedundancy'
  #-}
