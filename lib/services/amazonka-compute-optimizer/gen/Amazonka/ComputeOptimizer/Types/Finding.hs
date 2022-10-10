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
-- Module      : Amazonka.ComputeOptimizer.Types.Finding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.Finding
  ( Finding
      ( ..,
        Finding_NotOptimized,
        Finding_Optimized,
        Finding_Overprovisioned,
        Finding_Underprovisioned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype Finding = Finding' {fromFinding :: Core.Text}
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

pattern Finding_NotOptimized :: Finding
pattern Finding_NotOptimized = Finding' "NotOptimized"

pattern Finding_Optimized :: Finding
pattern Finding_Optimized = Finding' "Optimized"

pattern Finding_Overprovisioned :: Finding
pattern Finding_Overprovisioned = Finding' "Overprovisioned"

pattern Finding_Underprovisioned :: Finding
pattern Finding_Underprovisioned = Finding' "Underprovisioned"

{-# COMPLETE
  Finding_NotOptimized,
  Finding_Optimized,
  Finding_Overprovisioned,
  Finding_Underprovisioned,
  Finding'
  #-}
