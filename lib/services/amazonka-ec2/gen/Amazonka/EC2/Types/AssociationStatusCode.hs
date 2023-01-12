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
-- Module      : Amazonka.EC2.Types.AssociationStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AssociationStatusCode
  ( AssociationStatusCode
      ( ..,
        AssociationStatusCode_Associated,
        AssociationStatusCode_Associating,
        AssociationStatusCode_Association_failed,
        AssociationStatusCode_Disassociated,
        AssociationStatusCode_Disassociating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AssociationStatusCode = AssociationStatusCode'
  { fromAssociationStatusCode ::
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

pattern AssociationStatusCode_Associated :: AssociationStatusCode
pattern AssociationStatusCode_Associated = AssociationStatusCode' "associated"

pattern AssociationStatusCode_Associating :: AssociationStatusCode
pattern AssociationStatusCode_Associating = AssociationStatusCode' "associating"

pattern AssociationStatusCode_Association_failed :: AssociationStatusCode
pattern AssociationStatusCode_Association_failed = AssociationStatusCode' "association-failed"

pattern AssociationStatusCode_Disassociated :: AssociationStatusCode
pattern AssociationStatusCode_Disassociated = AssociationStatusCode' "disassociated"

pattern AssociationStatusCode_Disassociating :: AssociationStatusCode
pattern AssociationStatusCode_Disassociating = AssociationStatusCode' "disassociating"

{-# COMPLETE
  AssociationStatusCode_Associated,
  AssociationStatusCode_Associating,
  AssociationStatusCode_Association_failed,
  AssociationStatusCode_Disassociated,
  AssociationStatusCode_Disassociating,
  AssociationStatusCode'
  #-}
