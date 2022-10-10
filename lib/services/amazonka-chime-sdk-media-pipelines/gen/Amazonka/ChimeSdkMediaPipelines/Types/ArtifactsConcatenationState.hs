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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ArtifactsConcatenationState
  ( ArtifactsConcatenationState
      ( ..,
        ArtifactsConcatenationState_Disabled,
        ArtifactsConcatenationState_Enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ArtifactsConcatenationState = ArtifactsConcatenationState'
  { fromArtifactsConcatenationState ::
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

pattern ArtifactsConcatenationState_Disabled :: ArtifactsConcatenationState
pattern ArtifactsConcatenationState_Disabled = ArtifactsConcatenationState' "Disabled"

pattern ArtifactsConcatenationState_Enabled :: ArtifactsConcatenationState
pattern ArtifactsConcatenationState_Enabled = ArtifactsConcatenationState' "Enabled"

{-# COMPLETE
  ArtifactsConcatenationState_Disabled,
  ArtifactsConcatenationState_Enabled,
  ArtifactsConcatenationState'
  #-}
