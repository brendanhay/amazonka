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
-- Module      : Network.AWS.MediaPackage.Types.AdTriggersElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.AdTriggersElement
  ( AdTriggersElement
      ( ..,
        AdTriggersElement_BREAK,
        AdTriggersElement_DISTRIBUTOR_ADVERTISEMENT,
        AdTriggersElement_DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY,
        AdTriggersElement_DISTRIBUTOR_PLACEMENT_OPPORTUNITY,
        AdTriggersElement_PROVIDER_ADVERTISEMENT,
        AdTriggersElement_PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY,
        AdTriggersElement_PROVIDER_PLACEMENT_OPPORTUNITY,
        AdTriggersElement_SPLICE_INSERT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AdTriggersElement = AdTriggersElement'
  { fromAdTriggersElement ::
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

pattern AdTriggersElement_BREAK :: AdTriggersElement
pattern AdTriggersElement_BREAK = AdTriggersElement' "BREAK"

pattern AdTriggersElement_DISTRIBUTOR_ADVERTISEMENT :: AdTriggersElement
pattern AdTriggersElement_DISTRIBUTOR_ADVERTISEMENT = AdTriggersElement' "DISTRIBUTOR_ADVERTISEMENT"

pattern AdTriggersElement_DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY :: AdTriggersElement
pattern AdTriggersElement_DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY = AdTriggersElement' "DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern AdTriggersElement_DISTRIBUTOR_PLACEMENT_OPPORTUNITY :: AdTriggersElement
pattern AdTriggersElement_DISTRIBUTOR_PLACEMENT_OPPORTUNITY = AdTriggersElement' "DISTRIBUTOR_PLACEMENT_OPPORTUNITY"

pattern AdTriggersElement_PROVIDER_ADVERTISEMENT :: AdTriggersElement
pattern AdTriggersElement_PROVIDER_ADVERTISEMENT = AdTriggersElement' "PROVIDER_ADVERTISEMENT"

pattern AdTriggersElement_PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY :: AdTriggersElement
pattern AdTriggersElement_PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY = AdTriggersElement' "PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern AdTriggersElement_PROVIDER_PLACEMENT_OPPORTUNITY :: AdTriggersElement
pattern AdTriggersElement_PROVIDER_PLACEMENT_OPPORTUNITY = AdTriggersElement' "PROVIDER_PLACEMENT_OPPORTUNITY"

pattern AdTriggersElement_SPLICE_INSERT :: AdTriggersElement
pattern AdTriggersElement_SPLICE_INSERT = AdTriggersElement' "SPLICE_INSERT"

{-# COMPLETE
  AdTriggersElement_BREAK,
  AdTriggersElement_DISTRIBUTOR_ADVERTISEMENT,
  AdTriggersElement_DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY,
  AdTriggersElement_DISTRIBUTOR_PLACEMENT_OPPORTUNITY,
  AdTriggersElement_PROVIDER_ADVERTISEMENT,
  AdTriggersElement_PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY,
  AdTriggersElement_PROVIDER_PLACEMENT_OPPORTUNITY,
  AdTriggersElement_SPLICE_INSERT,
  AdTriggersElement'
  #-}
