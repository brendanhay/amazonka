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
-- Module      : Network.AWS.MediaConvert.Types.JobPhase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobPhase
  ( JobPhase
      ( ..,
        JobPhase_PROBING,
        JobPhase_TRANSCODING,
        JobPhase_UPLOADING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | A job\'s phase can be PROBING, TRANSCODING OR UPLOADING
newtype JobPhase = JobPhase'
  { fromJobPhase ::
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

pattern JobPhase_PROBING :: JobPhase
pattern JobPhase_PROBING = JobPhase' "PROBING"

pattern JobPhase_TRANSCODING :: JobPhase
pattern JobPhase_TRANSCODING = JobPhase' "TRANSCODING"

pattern JobPhase_UPLOADING :: JobPhase
pattern JobPhase_UPLOADING = JobPhase' "UPLOADING"

{-# COMPLETE
  JobPhase_PROBING,
  JobPhase_TRANSCODING,
  JobPhase_UPLOADING,
  JobPhase'
  #-}
