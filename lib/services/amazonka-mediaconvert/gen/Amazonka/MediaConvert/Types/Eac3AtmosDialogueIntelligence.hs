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
-- Module      : Amazonka.MediaConvert.Types.Eac3AtmosDialogueIntelligence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3AtmosDialogueIntelligence
  ( Eac3AtmosDialogueIntelligence
      ( ..,
        Eac3AtmosDialogueIntelligence_DISABLED,
        Eac3AtmosDialogueIntelligence_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue
-- analysis.
newtype Eac3AtmosDialogueIntelligence = Eac3AtmosDialogueIntelligence'
  { fromEac3AtmosDialogueIntelligence ::
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

pattern Eac3AtmosDialogueIntelligence_DISABLED :: Eac3AtmosDialogueIntelligence
pattern Eac3AtmosDialogueIntelligence_DISABLED = Eac3AtmosDialogueIntelligence' "DISABLED"

pattern Eac3AtmosDialogueIntelligence_ENABLED :: Eac3AtmosDialogueIntelligence
pattern Eac3AtmosDialogueIntelligence_ENABLED = Eac3AtmosDialogueIntelligence' "ENABLED"

{-# COMPLETE
  Eac3AtmosDialogueIntelligence_DISABLED,
  Eac3AtmosDialogueIntelligence_ENABLED,
  Eac3AtmosDialogueIntelligence'
  #-}
