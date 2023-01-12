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
-- Module      : Amazonka.Chime.Types.TranscribeMedicalSpecialty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.TranscribeMedicalSpecialty
  ( TranscribeMedicalSpecialty
      ( ..,
        TranscribeMedicalSpecialty_CARDIOLOGY,
        TranscribeMedicalSpecialty_NEUROLOGY,
        TranscribeMedicalSpecialty_ONCOLOGY,
        TranscribeMedicalSpecialty_PRIMARYCARE,
        TranscribeMedicalSpecialty_RADIOLOGY,
        TranscribeMedicalSpecialty_UROLOGY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TranscribeMedicalSpecialty = TranscribeMedicalSpecialty'
  { fromTranscribeMedicalSpecialty ::
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

pattern TranscribeMedicalSpecialty_CARDIOLOGY :: TranscribeMedicalSpecialty
pattern TranscribeMedicalSpecialty_CARDIOLOGY = TranscribeMedicalSpecialty' "CARDIOLOGY"

pattern TranscribeMedicalSpecialty_NEUROLOGY :: TranscribeMedicalSpecialty
pattern TranscribeMedicalSpecialty_NEUROLOGY = TranscribeMedicalSpecialty' "NEUROLOGY"

pattern TranscribeMedicalSpecialty_ONCOLOGY :: TranscribeMedicalSpecialty
pattern TranscribeMedicalSpecialty_ONCOLOGY = TranscribeMedicalSpecialty' "ONCOLOGY"

pattern TranscribeMedicalSpecialty_PRIMARYCARE :: TranscribeMedicalSpecialty
pattern TranscribeMedicalSpecialty_PRIMARYCARE = TranscribeMedicalSpecialty' "PRIMARYCARE"

pattern TranscribeMedicalSpecialty_RADIOLOGY :: TranscribeMedicalSpecialty
pattern TranscribeMedicalSpecialty_RADIOLOGY = TranscribeMedicalSpecialty' "RADIOLOGY"

pattern TranscribeMedicalSpecialty_UROLOGY :: TranscribeMedicalSpecialty
pattern TranscribeMedicalSpecialty_UROLOGY = TranscribeMedicalSpecialty' "UROLOGY"

{-# COMPLETE
  TranscribeMedicalSpecialty_CARDIOLOGY,
  TranscribeMedicalSpecialty_NEUROLOGY,
  TranscribeMedicalSpecialty_ONCOLOGY,
  TranscribeMedicalSpecialty_PRIMARYCARE,
  TranscribeMedicalSpecialty_RADIOLOGY,
  TranscribeMedicalSpecialty_UROLOGY,
  TranscribeMedicalSpecialty'
  #-}
