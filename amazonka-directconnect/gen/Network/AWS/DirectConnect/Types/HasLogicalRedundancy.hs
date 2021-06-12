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

import qualified Network.AWS.Core as Core

newtype HasLogicalRedundancy = HasLogicalRedundancy'
  { fromHasLogicalRedundancy ::
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
