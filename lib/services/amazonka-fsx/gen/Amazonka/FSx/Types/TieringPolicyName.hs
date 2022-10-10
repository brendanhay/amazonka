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
-- Module      : Amazonka.FSx.Types.TieringPolicyName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.TieringPolicyName
  ( TieringPolicyName
      ( ..,
        TieringPolicyName_ALL,
        TieringPolicyName_AUTO,
        TieringPolicyName_NONE,
        TieringPolicyName_SNAPSHOT_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TieringPolicyName = TieringPolicyName'
  { fromTieringPolicyName ::
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

pattern TieringPolicyName_ALL :: TieringPolicyName
pattern TieringPolicyName_ALL = TieringPolicyName' "ALL"

pattern TieringPolicyName_AUTO :: TieringPolicyName
pattern TieringPolicyName_AUTO = TieringPolicyName' "AUTO"

pattern TieringPolicyName_NONE :: TieringPolicyName
pattern TieringPolicyName_NONE = TieringPolicyName' "NONE"

pattern TieringPolicyName_SNAPSHOT_ONLY :: TieringPolicyName
pattern TieringPolicyName_SNAPSHOT_ONLY = TieringPolicyName' "SNAPSHOT_ONLY"

{-# COMPLETE
  TieringPolicyName_ALL,
  TieringPolicyName_AUTO,
  TieringPolicyName_NONE,
  TieringPolicyName_SNAPSHOT_ONLY,
  TieringPolicyName'
  #-}
