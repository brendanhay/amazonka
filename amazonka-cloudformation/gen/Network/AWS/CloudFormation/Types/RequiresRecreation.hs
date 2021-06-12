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
-- Module      : Network.AWS.CloudFormation.Types.RequiresRecreation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.RequiresRecreation
  ( RequiresRecreation
      ( ..,
        RequiresRecreation_Always,
        RequiresRecreation_Conditionally,
        RequiresRecreation_Never
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype RequiresRecreation = RequiresRecreation'
  { fromRequiresRecreation ::
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

pattern RequiresRecreation_Always :: RequiresRecreation
pattern RequiresRecreation_Always = RequiresRecreation' "Always"

pattern RequiresRecreation_Conditionally :: RequiresRecreation
pattern RequiresRecreation_Conditionally = RequiresRecreation' "Conditionally"

pattern RequiresRecreation_Never :: RequiresRecreation
pattern RequiresRecreation_Never = RequiresRecreation' "Never"

{-# COMPLETE
  RequiresRecreation_Always,
  RequiresRecreation_Conditionally,
  RequiresRecreation_Never,
  RequiresRecreation'
  #-}
