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
-- Module      : Amazonka.CloudFormation.Types.ChangeSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ChangeSource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeSource = ChangeSource'
  { fromChangeSource ::
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
