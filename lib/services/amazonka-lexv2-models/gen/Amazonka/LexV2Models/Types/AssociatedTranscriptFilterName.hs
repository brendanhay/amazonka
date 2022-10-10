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
-- Module      : Amazonka.LexV2Models.Types.AssociatedTranscriptFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AssociatedTranscriptFilterName
  ( AssociatedTranscriptFilterName
      ( ..,
        AssociatedTranscriptFilterName_IntentId,
        AssociatedTranscriptFilterName_SlotTypeId
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AssociatedTranscriptFilterName = AssociatedTranscriptFilterName'
  { fromAssociatedTranscriptFilterName ::
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

pattern AssociatedTranscriptFilterName_IntentId :: AssociatedTranscriptFilterName
pattern AssociatedTranscriptFilterName_IntentId = AssociatedTranscriptFilterName' "IntentId"

pattern AssociatedTranscriptFilterName_SlotTypeId :: AssociatedTranscriptFilterName
pattern AssociatedTranscriptFilterName_SlotTypeId = AssociatedTranscriptFilterName' "SlotTypeId"

{-# COMPLETE
  AssociatedTranscriptFilterName_IntentId,
  AssociatedTranscriptFilterName_SlotTypeId,
  AssociatedTranscriptFilterName'
  #-}
