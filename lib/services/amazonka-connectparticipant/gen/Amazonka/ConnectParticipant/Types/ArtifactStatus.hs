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
-- Module      : Amazonka.ConnectParticipant.Types.ArtifactStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.ArtifactStatus
  ( ArtifactStatus
      ( ..,
        ArtifactStatus_APPROVED,
        ArtifactStatus_IN_PROGRESS,
        ArtifactStatus_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ArtifactStatus = ArtifactStatus'
  { fromArtifactStatus ::
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

pattern ArtifactStatus_APPROVED :: ArtifactStatus
pattern ArtifactStatus_APPROVED = ArtifactStatus' "APPROVED"

pattern ArtifactStatus_IN_PROGRESS :: ArtifactStatus
pattern ArtifactStatus_IN_PROGRESS = ArtifactStatus' "IN_PROGRESS"

pattern ArtifactStatus_REJECTED :: ArtifactStatus
pattern ArtifactStatus_REJECTED = ArtifactStatus' "REJECTED"

{-# COMPLETE
  ArtifactStatus_APPROVED,
  ArtifactStatus_IN_PROGRESS,
  ArtifactStatus_REJECTED,
  ArtifactStatus'
  #-}
