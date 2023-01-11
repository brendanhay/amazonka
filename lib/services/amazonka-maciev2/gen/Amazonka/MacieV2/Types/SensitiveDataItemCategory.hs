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
-- Module      : Amazonka.MacieV2.Types.SensitiveDataItemCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SensitiveDataItemCategory
  ( SensitiveDataItemCategory
      ( ..,
        SensitiveDataItemCategory_CREDENTIALS,
        SensitiveDataItemCategory_CUSTOM_IDENTIFIER,
        SensitiveDataItemCategory_FINANCIAL_INFORMATION,
        SensitiveDataItemCategory_PERSONAL_INFORMATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a finding, the category of sensitive data that was detected and
-- produced the finding. For a managed data identifier, the category of
-- sensitive data that the managed data identifier detects. Possible values
-- are:
newtype SensitiveDataItemCategory = SensitiveDataItemCategory'
  { fromSensitiveDataItemCategory ::
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

pattern SensitiveDataItemCategory_CREDENTIALS :: SensitiveDataItemCategory
pattern SensitiveDataItemCategory_CREDENTIALS = SensitiveDataItemCategory' "CREDENTIALS"

pattern SensitiveDataItemCategory_CUSTOM_IDENTIFIER :: SensitiveDataItemCategory
pattern SensitiveDataItemCategory_CUSTOM_IDENTIFIER = SensitiveDataItemCategory' "CUSTOM_IDENTIFIER"

pattern SensitiveDataItemCategory_FINANCIAL_INFORMATION :: SensitiveDataItemCategory
pattern SensitiveDataItemCategory_FINANCIAL_INFORMATION = SensitiveDataItemCategory' "FINANCIAL_INFORMATION"

pattern SensitiveDataItemCategory_PERSONAL_INFORMATION :: SensitiveDataItemCategory
pattern SensitiveDataItemCategory_PERSONAL_INFORMATION = SensitiveDataItemCategory' "PERSONAL_INFORMATION"

{-# COMPLETE
  SensitiveDataItemCategory_CREDENTIALS,
  SensitiveDataItemCategory_CUSTOM_IDENTIFIER,
  SensitiveDataItemCategory_FINANCIAL_INFORMATION,
  SensitiveDataItemCategory_PERSONAL_INFORMATION,
  SensitiveDataItemCategory'
  #-}
