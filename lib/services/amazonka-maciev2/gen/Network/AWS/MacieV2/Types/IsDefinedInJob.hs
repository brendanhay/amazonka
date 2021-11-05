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
-- Module      : Network.AWS.MacieV2.Types.IsDefinedInJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.IsDefinedInJob
  ( IsDefinedInJob
      ( ..,
        IsDefinedInJob_FALSE,
        IsDefinedInJob_TRUE,
        IsDefinedInJob_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype IsDefinedInJob = IsDefinedInJob'
  { fromIsDefinedInJob ::
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

pattern IsDefinedInJob_FALSE :: IsDefinedInJob
pattern IsDefinedInJob_FALSE = IsDefinedInJob' "FALSE"

pattern IsDefinedInJob_TRUE :: IsDefinedInJob
pattern IsDefinedInJob_TRUE = IsDefinedInJob' "TRUE"

pattern IsDefinedInJob_UNKNOWN :: IsDefinedInJob
pattern IsDefinedInJob_UNKNOWN = IsDefinedInJob' "UNKNOWN"

{-# COMPLETE
  IsDefinedInJob_FALSE,
  IsDefinedInJob_TRUE,
  IsDefinedInJob_UNKNOWN,
  IsDefinedInJob'
  #-}
