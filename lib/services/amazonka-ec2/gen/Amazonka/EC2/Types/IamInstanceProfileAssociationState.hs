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
-- Module      : Amazonka.EC2.Types.IamInstanceProfileAssociationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IamInstanceProfileAssociationState
  ( IamInstanceProfileAssociationState
      ( ..,
        IamInstanceProfileAssociationState_Associated,
        IamInstanceProfileAssociationState_Associating,
        IamInstanceProfileAssociationState_Disassociated,
        IamInstanceProfileAssociationState_Disassociating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype IamInstanceProfileAssociationState = IamInstanceProfileAssociationState'
  { fromIamInstanceProfileAssociationState ::
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
