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
-- Module      : Amazonka.SESV2.Types.ImportDestinationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ImportDestinationType
  ( ImportDestinationType
      ( ..,
        ImportDestinationType_CONTACT_LIST,
        ImportDestinationType_SUPPRESSION_LIST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The destination of the import job, which can be used to list import jobs
-- that have a certain @ImportDestinationType@.
newtype ImportDestinationType = ImportDestinationType'
  { fromImportDestinationType ::
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

pattern ImportDestinationType_CONTACT_LIST :: ImportDestinationType
pattern ImportDestinationType_CONTACT_LIST = ImportDestinationType' "CONTACT_LIST"

pattern ImportDestinationType_SUPPRESSION_LIST :: ImportDestinationType
pattern ImportDestinationType_SUPPRESSION_LIST = ImportDestinationType' "SUPPRESSION_LIST"

{-# COMPLETE
  ImportDestinationType_CONTACT_LIST,
  ImportDestinationType_SUPPRESSION_LIST,
  ImportDestinationType'
  #-}
