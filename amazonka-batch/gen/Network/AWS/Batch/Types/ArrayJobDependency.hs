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
-- Module      : Network.AWS.Batch.Types.ArrayJobDependency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayJobDependency
  ( ArrayJobDependency
      ( ..,
        ArrayJobDependency_N_TO_N,
        ArrayJobDependency_SEQUENTIAL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ArrayJobDependency = ArrayJobDependency'
  { fromArrayJobDependency ::
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

pattern ArrayJobDependency_N_TO_N :: ArrayJobDependency
pattern ArrayJobDependency_N_TO_N = ArrayJobDependency' "N_TO_N"

pattern ArrayJobDependency_SEQUENTIAL :: ArrayJobDependency
pattern ArrayJobDependency_SEQUENTIAL = ArrayJobDependency' "SEQUENTIAL"

{-# COMPLETE
  ArrayJobDependency_N_TO_N,
  ArrayJobDependency_SEQUENTIAL,
  ArrayJobDependency'
  #-}
