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

import qualified Network.AWS.Prelude as Prelude

newtype ParameterTier = ParameterTier'
  { fromParameterTier ::
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
