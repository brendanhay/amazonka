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
-- Module      : Amazonka.FSx.Types.OntapVolumeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OntapVolumeType
  ( OntapVolumeType
      ( ..,
        OntapVolumeType_DP,
        OntapVolumeType_LS,
        OntapVolumeType_RW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OntapVolumeType = OntapVolumeType'
  { fromOntapVolumeType ::
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

pattern OntapVolumeType_DP :: OntapVolumeType
pattern OntapVolumeType_DP = OntapVolumeType' "DP"

pattern OntapVolumeType_LS :: OntapVolumeType
pattern OntapVolumeType_LS = OntapVolumeType' "LS"

pattern OntapVolumeType_RW :: OntapVolumeType
pattern OntapVolumeType_RW = OntapVolumeType' "RW"

{-# COMPLETE
  OntapVolumeType_DP,
  OntapVolumeType_LS,
  OntapVolumeType_RW,
  OntapVolumeType'
  #-}
