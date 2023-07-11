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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineValueLabelRelativePosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineValueLabelRelativePosition
  ( ReferenceLineValueLabelRelativePosition
      ( ..,
        ReferenceLineValueLabelRelativePosition_AFTER_CUSTOM_LABEL,
        ReferenceLineValueLabelRelativePosition_BEFORE_CUSTOM_LABEL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReferenceLineValueLabelRelativePosition = ReferenceLineValueLabelRelativePosition'
  { fromReferenceLineValueLabelRelativePosition ::
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

pattern ReferenceLineValueLabelRelativePosition_AFTER_CUSTOM_LABEL :: ReferenceLineValueLabelRelativePosition
pattern ReferenceLineValueLabelRelativePosition_AFTER_CUSTOM_LABEL = ReferenceLineValueLabelRelativePosition' "AFTER_CUSTOM_LABEL"

pattern ReferenceLineValueLabelRelativePosition_BEFORE_CUSTOM_LABEL :: ReferenceLineValueLabelRelativePosition
pattern ReferenceLineValueLabelRelativePosition_BEFORE_CUSTOM_LABEL = ReferenceLineValueLabelRelativePosition' "BEFORE_CUSTOM_LABEL"

{-# COMPLETE
  ReferenceLineValueLabelRelativePosition_AFTER_CUSTOM_LABEL,
  ReferenceLineValueLabelRelativePosition_BEFORE_CUSTOM_LABEL,
  ReferenceLineValueLabelRelativePosition'
  #-}
