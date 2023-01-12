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
-- Module      : Amazonka.CostAndUsageReport.Types.AdditionalArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostAndUsageReport.Types.AdditionalArtifact
  ( AdditionalArtifact
      ( ..,
        AdditionalArtifact_ATHENA,
        AdditionalArtifact_QUICKSIGHT,
        AdditionalArtifact_REDSHIFT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The types of manifest that you want AWS to create for this report.
newtype AdditionalArtifact = AdditionalArtifact'
  { fromAdditionalArtifact ::
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

pattern AdditionalArtifact_ATHENA :: AdditionalArtifact
pattern AdditionalArtifact_ATHENA = AdditionalArtifact' "ATHENA"

pattern AdditionalArtifact_QUICKSIGHT :: AdditionalArtifact
pattern AdditionalArtifact_QUICKSIGHT = AdditionalArtifact' "QUICKSIGHT"

pattern AdditionalArtifact_REDSHIFT :: AdditionalArtifact
pattern AdditionalArtifact_REDSHIFT = AdditionalArtifact' "REDSHIFT"

{-# COMPLETE
  AdditionalArtifact_ATHENA,
  AdditionalArtifact_QUICKSIGHT,
  AdditionalArtifact_REDSHIFT,
  AdditionalArtifact'
  #-}
