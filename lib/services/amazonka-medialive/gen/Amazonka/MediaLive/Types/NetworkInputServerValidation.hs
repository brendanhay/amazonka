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
-- Module      : Amazonka.MediaLive.Types.NetworkInputServerValidation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NetworkInputServerValidation
  ( NetworkInputServerValidation
      ( ..,
        NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME,
        NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Network Input Server Validation
newtype NetworkInputServerValidation = NetworkInputServerValidation'
  { fromNetworkInputServerValidation ::
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

pattern NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME :: NetworkInputServerValidation
pattern NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME = NetworkInputServerValidation' "CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME"

pattern NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_ONLY :: NetworkInputServerValidation
pattern NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_ONLY = NetworkInputServerValidation' "CHECK_CRYPTOGRAPHY_ONLY"

{-# COMPLETE
  NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME,
  NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_ONLY,
  NetworkInputServerValidation'
  #-}
