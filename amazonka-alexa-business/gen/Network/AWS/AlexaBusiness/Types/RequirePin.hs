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
-- Module      : Network.AWS.AlexaBusiness.Types.RequirePin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RequirePin
  ( RequirePin
      ( ..,
        RequirePin_NO,
        RequirePin_OPTIONAL,
        RequirePin_YES
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RequirePin = RequirePin'
  { fromRequirePin ::
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

pattern RequirePin_NO :: RequirePin
pattern RequirePin_NO = RequirePin' "NO"

pattern RequirePin_OPTIONAL :: RequirePin
pattern RequirePin_OPTIONAL = RequirePin' "OPTIONAL"

pattern RequirePin_YES :: RequirePin
pattern RequirePin_YES = RequirePin' "YES"

{-# COMPLETE
  RequirePin_NO,
  RequirePin_OPTIONAL,
  RequirePin_YES,
  RequirePin'
  #-}
