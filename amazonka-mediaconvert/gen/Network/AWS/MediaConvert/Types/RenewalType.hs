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
-- Module      : Network.AWS.MediaConvert.Types.RenewalType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.RenewalType
  ( RenewalType
      ( ..,
        RenewalType_AUTO_RENEW,
        RenewalType_EXPIRE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Specifies whether the term of your reserved queue pricing plan is
-- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
-- the term.
newtype RenewalType = RenewalType'
  { fromRenewalType ::
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

pattern RenewalType_AUTO_RENEW :: RenewalType
pattern RenewalType_AUTO_RENEW = RenewalType' "AUTO_RENEW"

pattern RenewalType_EXPIRE :: RenewalType
pattern RenewalType_EXPIRE = RenewalType' "EXPIRE"

{-# COMPLETE
  RenewalType_AUTO_RENEW,
  RenewalType_EXPIRE,
  RenewalType'
  #-}
