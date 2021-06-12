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
-- Module      : Network.AWS.EC2.Types.IamInstanceProfileAssociationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IamInstanceProfileAssociationState
  ( IamInstanceProfileAssociationState
      ( ..,
        IamInstanceProfileAssociationState_Associated,
        IamInstanceProfileAssociationState_Associating,
        IamInstanceProfileAssociationState_Disassociated,
        IamInstanceProfileAssociationState_Disassociating
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype IamInstanceProfileAssociationState = IamInstanceProfileAssociationState'
  { fromIamInstanceProfileAssociationState ::
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

pattern IamInstanceProfileAssociationState_Associated :: IamInstanceProfileAssociationState
pattern IamInstanceProfileAssociationState_Associated = IamInstanceProfileAssociationState' "associated"

pattern IamInstanceProfileAssociationState_Associating :: IamInstanceProfileAssociationState
pattern IamInstanceProfileAssociationState_Associating = IamInstanceProfileAssociationState' "associating"

pattern IamInstanceProfileAssociationState_Disassociated :: IamInstanceProfileAssociationState
pattern IamInstanceProfileAssociationState_Disassociated = IamInstanceProfileAssociationState' "disassociated"

pattern IamInstanceProfileAssociationState_Disassociating :: IamInstanceProfileAssociationState
pattern IamInstanceProfileAssociationState_Disassociating = IamInstanceProfileAssociationState' "disassociating"

{-# COMPLETE
  IamInstanceProfileAssociationState_Associated,
  IamInstanceProfileAssociationState_Associating,
  IamInstanceProfileAssociationState_Disassociated,
  IamInstanceProfileAssociationState_Disassociating,
  IamInstanceProfileAssociationState'
  #-}
