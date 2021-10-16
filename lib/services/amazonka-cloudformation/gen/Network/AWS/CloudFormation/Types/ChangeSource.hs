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
-- Module      : Network.AWS.CloudFormation.Types.ChangeSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSource
  ( ChangeSource
      ( ..,
        ChangeSource_Automatic,
        ChangeSource_DirectModification,
        ChangeSource_ParameterReference,
        ChangeSource_ResourceAttribute,
        ChangeSource_ResourceReference
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ChangeSource = ChangeSource'
  { fromChangeSource ::
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

pattern ChangeSource_Automatic :: ChangeSource
pattern ChangeSource_Automatic = ChangeSource' "Automatic"

pattern ChangeSource_DirectModification :: ChangeSource
pattern ChangeSource_DirectModification = ChangeSource' "DirectModification"

pattern ChangeSource_ParameterReference :: ChangeSource
pattern ChangeSource_ParameterReference = ChangeSource' "ParameterReference"

pattern ChangeSource_ResourceAttribute :: ChangeSource
pattern ChangeSource_ResourceAttribute = ChangeSource' "ResourceAttribute"

pattern ChangeSource_ResourceReference :: ChangeSource
pattern ChangeSource_ResourceReference = ChangeSource' "ResourceReference"

{-# COMPLETE
  ChangeSource_Automatic,
  ChangeSource_DirectModification,
  ChangeSource_ParameterReference,
  ChangeSource_ResourceAttribute,
  ChangeSource_ResourceReference,
  ChangeSource'
  #-}
