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
-- Module      : Network.AWS.SSM.Types.ParameterTier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterTier
  ( ParameterTier
      ( ..,
        ParameterTier_Advanced,
        ParameterTier_Intelligent_Tiering,
        ParameterTier_Standard
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ParameterTier = ParameterTier'
  { fromParameterTier ::
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

pattern ParameterTier_Advanced :: ParameterTier
pattern ParameterTier_Advanced = ParameterTier' "Advanced"

pattern ParameterTier_Intelligent_Tiering :: ParameterTier
pattern ParameterTier_Intelligent_Tiering = ParameterTier' "Intelligent-Tiering"

pattern ParameterTier_Standard :: ParameterTier
pattern ParameterTier_Standard = ParameterTier' "Standard"

{-# COMPLETE
  ParameterTier_Advanced,
  ParameterTier_Intelligent_Tiering,
  ParameterTier_Standard,
  ParameterTier'
  #-}
