{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
  ( Eac3AtmosDialogueIntelligence
      ( Eac3AtmosDialogueIntelligence',
        Eac3AtmosDialogueIntelligenceEnabled,
        Eac3AtmosDialogueIntelligenceDisabled,
        fromEac3AtmosDialogueIntelligence
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
newtype Eac3AtmosDialogueIntelligence = Eac3AtmosDialogueIntelligence'
  { fromEac3AtmosDialogueIntelligence ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Eac3AtmosDialogueIntelligenceEnabled :: Eac3AtmosDialogueIntelligence
pattern Eac3AtmosDialogueIntelligenceEnabled = Eac3AtmosDialogueIntelligence' "ENABLED"

pattern Eac3AtmosDialogueIntelligenceDisabled :: Eac3AtmosDialogueIntelligence
pattern Eac3AtmosDialogueIntelligenceDisabled = Eac3AtmosDialogueIntelligence' "DISABLED"

{-# COMPLETE
  Eac3AtmosDialogueIntelligenceEnabled,
  Eac3AtmosDialogueIntelligenceDisabled,
  Eac3AtmosDialogueIntelligence'
  #-}
