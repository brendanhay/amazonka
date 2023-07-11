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
-- Module      : Amazonka.SSM.Types.ParameterTier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ParameterTier
  ( ParameterTier
      ( ..,
        ParameterTier_Advanced,
        ParameterTier_Intelligent_Tiering,
        ParameterTier_Standard
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ParameterTier = ParameterTier'
  { fromParameterTier ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
