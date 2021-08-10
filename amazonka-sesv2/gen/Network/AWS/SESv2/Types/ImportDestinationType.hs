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
-- Module      : Network.AWS.SESv2.Types.ImportDestinationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.ImportDestinationType
  ( ImportDestinationType
      ( ..,
        ImportDestinationType_CONTACT_LIST,
        ImportDestinationType_SUPPRESSION_LIST
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The destination of the import job, which can be used to list import jobs
-- that have a certain @ImportDestinationType@.
newtype ImportDestinationType = ImportDestinationType'
  { fromImportDestinationType ::
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

pattern ImportDestinationType_CONTACT_LIST :: ImportDestinationType
pattern ImportDestinationType_CONTACT_LIST = ImportDestinationType' "CONTACT_LIST"

pattern ImportDestinationType_SUPPRESSION_LIST :: ImportDestinationType
pattern ImportDestinationType_SUPPRESSION_LIST = ImportDestinationType' "SUPPRESSION_LIST"

{-# COMPLETE
  ImportDestinationType_CONTACT_LIST,
  ImportDestinationType_SUPPRESSION_LIST,
  ImportDestinationType'
  #-}
