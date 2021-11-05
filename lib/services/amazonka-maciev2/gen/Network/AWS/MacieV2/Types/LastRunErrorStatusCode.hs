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
-- Module      : Network.AWS.MacieV2.Types.LastRunErrorStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.LastRunErrorStatusCode
  ( LastRunErrorStatusCode
      ( ..,
        LastRunErrorStatusCode_ERROR,
        LastRunErrorStatusCode_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specifies whether any account- or bucket-level access errors occurred
-- during the run of a one-time classification job or the most recent run
-- of a recurring classification job. Possible values are:
newtype LastRunErrorStatusCode = LastRunErrorStatusCode'
  { fromLastRunErrorStatusCode ::
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

pattern LastRunErrorStatusCode_ERROR :: LastRunErrorStatusCode
pattern LastRunErrorStatusCode_ERROR = LastRunErrorStatusCode' "ERROR"

pattern LastRunErrorStatusCode_NONE :: LastRunErrorStatusCode
pattern LastRunErrorStatusCode_NONE = LastRunErrorStatusCode' "NONE"

{-# COMPLETE
  LastRunErrorStatusCode_ERROR,
  LastRunErrorStatusCode_NONE,
  LastRunErrorStatusCode'
  #-}
