{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
  ( AdditionalArtifact
      ( ..,
        AdditionalArtifact_ATHENA,
        AdditionalArtifact_QUICKSIGHT,
        AdditionalArtifact_REDSHIFT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The types of manifest that you want AWS to create for this report.
newtype AdditionalArtifact = AdditionalArtifact'
  { fromAdditionalArtifact ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
