{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ParameterType
  ( ParameterType
    ( ParameterType'
    , ParameterTypeString
    , ParameterTypeStringList
    , ParameterTypeSecureString
    , fromParameterType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ParameterType = ParameterType'{fromParameterType ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern ParameterTypeString :: ParameterType
pattern ParameterTypeString = ParameterType' "String"

pattern ParameterTypeStringList :: ParameterType
pattern ParameterTypeStringList = ParameterType' "StringList"

pattern ParameterTypeSecureString :: ParameterType
pattern ParameterTypeSecureString = ParameterType' "SecureString"

{-# COMPLETE 
  ParameterTypeString,

  ParameterTypeStringList,

  ParameterTypeSecureString,
  ParameterType'
  #-}
