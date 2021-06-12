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
-- Module      : Network.AWS.MediaLive.Types.NetworkInputServerValidation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NetworkInputServerValidation
  ( NetworkInputServerValidation
      ( ..,
        NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_AND_VALIDATE_NAME,
        NetworkInputServerValidation_CHECK_CRYPTOGRAPHY_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Network Input Server Validation
newtype NetworkInputServerValidation = NetworkInputServerValidation'
  { fromNetworkInputServerValidation ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
