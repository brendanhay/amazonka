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
-- Module      : Network.AWS.CloudFormation.Types.ChangeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeAction
  ( ChangeAction
      ( ..,
        ChangeAction_Add,
        ChangeAction_Dynamic,
        ChangeAction_Import,
        ChangeAction_Modify,
        ChangeAction_Remove
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ChangeAction = ChangeAction'
  { fromChangeAction ::
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

pattern ChangeAction_Add :: ChangeAction
pattern ChangeAction_Add = ChangeAction' "Add"

pattern ChangeAction_Dynamic :: ChangeAction
pattern ChangeAction_Dynamic = ChangeAction' "Dynamic"

pattern ChangeAction_Import :: ChangeAction
pattern ChangeAction_Import = ChangeAction' "Import"

pattern ChangeAction_Modify :: ChangeAction
pattern ChangeAction_Modify = ChangeAction' "Modify"

pattern ChangeAction_Remove :: ChangeAction
pattern ChangeAction_Remove = ChangeAction' "Remove"

{-# COMPLETE
  ChangeAction_Add,
  ChangeAction_Dynamic,
  ChangeAction_Import,
  ChangeAction_Modify,
  ChangeAction_Remove,
  ChangeAction'
  #-}
