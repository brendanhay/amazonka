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
-- Module      : Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
  ( RequiredAttributeBehavior
      ( ..,
        RequiredAttributeBehavior_NOT_REQUIRED,
        RequiredAttributeBehavior_REQUIRED_ALWAYS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RequiredAttributeBehavior = RequiredAttributeBehavior'
  { fromRequiredAttributeBehavior ::
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

pattern RequiredAttributeBehavior_NOT_REQUIRED :: RequiredAttributeBehavior
pattern RequiredAttributeBehavior_NOT_REQUIRED = RequiredAttributeBehavior' "NOT_REQUIRED"

pattern RequiredAttributeBehavior_REQUIRED_ALWAYS :: RequiredAttributeBehavior
pattern RequiredAttributeBehavior_REQUIRED_ALWAYS = RequiredAttributeBehavior' "REQUIRED_ALWAYS"

{-# COMPLETE
  RequiredAttributeBehavior_NOT_REQUIRED,
  RequiredAttributeBehavior_REQUIRED_ALWAYS,
  RequiredAttributeBehavior'
  #-}
