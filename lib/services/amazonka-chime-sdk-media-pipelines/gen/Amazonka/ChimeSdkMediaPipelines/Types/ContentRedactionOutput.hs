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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.ContentRedactionOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.ContentRedactionOutput
  ( ContentRedactionOutput
      ( ..,
        ContentRedactionOutput_Redacted,
        ContentRedactionOutput_Redacted_and_unredacted
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContentRedactionOutput = ContentRedactionOutput'
  { fromContentRedactionOutput ::
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

pattern ContentRedactionOutput_Redacted :: ContentRedactionOutput
pattern ContentRedactionOutput_Redacted = ContentRedactionOutput' "redacted"

pattern ContentRedactionOutput_Redacted_and_unredacted :: ContentRedactionOutput
pattern ContentRedactionOutput_Redacted_and_unredacted = ContentRedactionOutput' "redacted_and_unredacted"

{-# COMPLETE
  ContentRedactionOutput_Redacted,
  ContentRedactionOutput_Redacted_and_unredacted,
  ContentRedactionOutput'
  #-}
