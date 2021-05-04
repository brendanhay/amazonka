{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Network Input Server Validation
newtype NetworkInputServerValidation = NetworkInputServerValidation'
  { fromNetworkInputServerValidation ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
