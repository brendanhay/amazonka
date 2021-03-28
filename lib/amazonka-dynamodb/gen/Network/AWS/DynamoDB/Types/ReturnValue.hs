{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReturnValue
  ( ReturnValue
    ( ReturnValue'
    , ReturnValueNone
    , ReturnValueAllOld
    , ReturnValueUpdatedOld
    , ReturnValueAllNew
    , ReturnValueUpdatedNew
    , fromReturnValue
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ReturnValue = ReturnValue'{fromReturnValue :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern ReturnValueNone :: ReturnValue
pattern ReturnValueNone = ReturnValue' "NONE"

pattern ReturnValueAllOld :: ReturnValue
pattern ReturnValueAllOld = ReturnValue' "ALL_OLD"

pattern ReturnValueUpdatedOld :: ReturnValue
pattern ReturnValueUpdatedOld = ReturnValue' "UPDATED_OLD"

pattern ReturnValueAllNew :: ReturnValue
pattern ReturnValueAllNew = ReturnValue' "ALL_NEW"

pattern ReturnValueUpdatedNew :: ReturnValue
pattern ReturnValueUpdatedNew = ReturnValue' "UPDATED_NEW"

{-# COMPLETE 
  ReturnValueNone,

  ReturnValueAllOld,

  ReturnValueUpdatedOld,

  ReturnValueAllNew,

  ReturnValueUpdatedNew,
  ReturnValue'
  #-}
