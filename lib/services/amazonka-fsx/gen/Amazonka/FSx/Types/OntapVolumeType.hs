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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype OntapVolumeType = OntapVolumeType'
  { fromOntapVolumeType ::
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
