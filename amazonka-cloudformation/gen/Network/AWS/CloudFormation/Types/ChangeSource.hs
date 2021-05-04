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

import qualified Network.AWS.Prelude as Prelude

newtype ChangeSource = ChangeSource'
  { fromChangeSource ::
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
