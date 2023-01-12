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
-- Module      : Amazonka.MacieV2.Types.LastRunErrorStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.LastRunErrorStatusCode
  ( LastRunErrorStatusCode
      ( ..,
        LastRunErrorStatusCode_ERROR,
        LastRunErrorStatusCode_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether any account- or bucket-level access errors occurred
-- during the run of a one-time classification job or the most recent run
-- of a recurring classification job. Possible values are:
newtype LastRunErrorStatusCode = LastRunErrorStatusCode'
  { fromLastRunErrorStatusCode ::
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

pattern LastRunErrorStatusCode_ERROR :: LastRunErrorStatusCode
pattern LastRunErrorStatusCode_ERROR = LastRunErrorStatusCode' "ERROR"

pattern LastRunErrorStatusCode_NONE :: LastRunErrorStatusCode
pattern LastRunErrorStatusCode_NONE = LastRunErrorStatusCode' "NONE"

{-# COMPLETE
  LastRunErrorStatusCode_ERROR,
  LastRunErrorStatusCode_NONE,
  LastRunErrorStatusCode'
  #-}
