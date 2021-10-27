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
-- Module      : Network.AWS.MediaTailor.Types.AccessType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaTailor.Types.AccessType
  ( AccessType
      ( ..,
        AccessType_S3_SIGV4,
        AccessType_SECRETS_MANAGER_ACCESS_TOKEN
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AccessType = AccessType'
  { fromAccessType ::
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

pattern AccessType_S3_SIGV4 :: AccessType
pattern AccessType_S3_SIGV4 = AccessType' "S3_SIGV4"

pattern AccessType_SECRETS_MANAGER_ACCESS_TOKEN :: AccessType
pattern AccessType_SECRETS_MANAGER_ACCESS_TOKEN = AccessType' "SECRETS_MANAGER_ACCESS_TOKEN"

{-# COMPLETE
  AccessType_S3_SIGV4,
  AccessType_SECRETS_MANAGER_ACCESS_TOKEN,
  AccessType'
  #-}
