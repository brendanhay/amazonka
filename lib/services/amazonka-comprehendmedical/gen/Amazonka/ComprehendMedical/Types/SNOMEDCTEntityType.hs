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
-- Module      : Amazonka.ComprehendMedical.Types.SNOMEDCTEntityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.SNOMEDCTEntityType
  ( SNOMEDCTEntityType
      ( ..,
        SNOMEDCTEntityType_DX_NAME,
        SNOMEDCTEntityType_PROCEDURE_NAME,
        SNOMEDCTEntityType_TEST_NAME,
        SNOMEDCTEntityType_TREATMENT_NAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SNOMEDCTEntityType = SNOMEDCTEntityType'
  { fromSNOMEDCTEntityType ::
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

pattern SNOMEDCTEntityType_DX_NAME :: SNOMEDCTEntityType
pattern SNOMEDCTEntityType_DX_NAME = SNOMEDCTEntityType' "DX_NAME"

pattern SNOMEDCTEntityType_PROCEDURE_NAME :: SNOMEDCTEntityType
pattern SNOMEDCTEntityType_PROCEDURE_NAME = SNOMEDCTEntityType' "PROCEDURE_NAME"

pattern SNOMEDCTEntityType_TEST_NAME :: SNOMEDCTEntityType
pattern SNOMEDCTEntityType_TEST_NAME = SNOMEDCTEntityType' "TEST_NAME"

pattern SNOMEDCTEntityType_TREATMENT_NAME :: SNOMEDCTEntityType
pattern SNOMEDCTEntityType_TREATMENT_NAME = SNOMEDCTEntityType' "TREATMENT_NAME"

{-# COMPLETE
  SNOMEDCTEntityType_DX_NAME,
  SNOMEDCTEntityType_PROCEDURE_NAME,
  SNOMEDCTEntityType_TEST_NAME,
  SNOMEDCTEntityType_TREATMENT_NAME,
  SNOMEDCTEntityType'
  #-}
