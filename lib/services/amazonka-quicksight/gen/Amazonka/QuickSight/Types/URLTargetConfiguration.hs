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
-- Module      : Amazonka.QuickSight.Types.URLTargetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.URLTargetConfiguration
  ( URLTargetConfiguration
      ( ..,
        URLTargetConfiguration_NEW_TAB,
        URLTargetConfiguration_NEW_WINDOW,
        URLTargetConfiguration_SAME_TAB
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype URLTargetConfiguration = URLTargetConfiguration'
  { fromURLTargetConfiguration ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern URLTargetConfiguration_NEW_TAB :: URLTargetConfiguration
pattern URLTargetConfiguration_NEW_TAB = URLTargetConfiguration' "NEW_TAB"

pattern URLTargetConfiguration_NEW_WINDOW :: URLTargetConfiguration
pattern URLTargetConfiguration_NEW_WINDOW = URLTargetConfiguration' "NEW_WINDOW"

pattern URLTargetConfiguration_SAME_TAB :: URLTargetConfiguration
pattern URLTargetConfiguration_SAME_TAB = URLTargetConfiguration' "SAME_TAB"

{-# COMPLETE
  URLTargetConfiguration_NEW_TAB,
  URLTargetConfiguration_NEW_WINDOW,
  URLTargetConfiguration_SAME_TAB,
  URLTargetConfiguration'
  #-}
