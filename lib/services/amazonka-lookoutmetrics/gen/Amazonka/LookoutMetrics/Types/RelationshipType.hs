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
-- Module      : Amazonka.LookoutMetrics.Types.RelationshipType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.RelationshipType
  ( RelationshipType
      ( ..,
        RelationshipType_CAUSE_OF_INPUT_ANOMALY_GROUP,
        RelationshipType_EFFECT_OF_INPUT_ANOMALY_GROUP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RelationshipType = RelationshipType'
  { fromRelationshipType ::
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

pattern RelationshipType_CAUSE_OF_INPUT_ANOMALY_GROUP :: RelationshipType
pattern RelationshipType_CAUSE_OF_INPUT_ANOMALY_GROUP = RelationshipType' "CAUSE_OF_INPUT_ANOMALY_GROUP"

pattern RelationshipType_EFFECT_OF_INPUT_ANOMALY_GROUP :: RelationshipType
pattern RelationshipType_EFFECT_OF_INPUT_ANOMALY_GROUP = RelationshipType' "EFFECT_OF_INPUT_ANOMALY_GROUP"

{-# COMPLETE
  RelationshipType_CAUSE_OF_INPUT_ANOMALY_GROUP,
  RelationshipType_EFFECT_OF_INPUT_ANOMALY_GROUP,
  RelationshipType'
  #-}
