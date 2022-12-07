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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTEntityCategory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTEntityCategory
  ( SNOMEDCTEntityCategory
      ( ..,
        SNOMEDCTEntityCategory_ANATOMY,
        SNOMEDCTEntityCategory_MEDICAL_CONDITION,
        SNOMEDCTEntityCategory_TEST_TREATMENT_PROCEDURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SNOMEDCTEntityCategory = SNOMEDCTEntityCategory'
  { fromSNOMEDCTEntityCategory ::
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

pattern SNOMEDCTEntityCategory_ANATOMY :: SNOMEDCTEntityCategory
pattern SNOMEDCTEntityCategory_ANATOMY = SNOMEDCTEntityCategory' "ANATOMY"

pattern SNOMEDCTEntityCategory_MEDICAL_CONDITION :: SNOMEDCTEntityCategory
pattern SNOMEDCTEntityCategory_MEDICAL_CONDITION = SNOMEDCTEntityCategory' "MEDICAL_CONDITION"

pattern SNOMEDCTEntityCategory_TEST_TREATMENT_PROCEDURE :: SNOMEDCTEntityCategory
pattern SNOMEDCTEntityCategory_TEST_TREATMENT_PROCEDURE = SNOMEDCTEntityCategory' "TEST_TREATMENT_PROCEDURE"

{-# COMPLETE
  SNOMEDCTEntityCategory_ANATOMY,
  SNOMEDCTEntityCategory_MEDICAL_CONDITION,
  SNOMEDCTEntityCategory_TEST_TREATMENT_PROCEDURE,
  SNOMEDCTEntityCategory'
  #-}
