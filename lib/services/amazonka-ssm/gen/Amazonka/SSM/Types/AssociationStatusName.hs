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
-- Module      : Amazonka.SSM.Types.AssociationStatusName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationStatusName
  ( AssociationStatusName
      ( ..,
        AssociationStatusName_Failed,
        AssociationStatusName_Pending,
        AssociationStatusName_Success
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssociationStatusName = AssociationStatusName'
  { fromAssociationStatusName ::
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

pattern AssociationStatusName_Failed :: AssociationStatusName
pattern AssociationStatusName_Failed = AssociationStatusName' "Failed"

pattern AssociationStatusName_Pending :: AssociationStatusName
pattern AssociationStatusName_Pending = AssociationStatusName' "Pending"

pattern AssociationStatusName_Success :: AssociationStatusName
pattern AssociationStatusName_Success = AssociationStatusName' "Success"

{-# COMPLETE
  AssociationStatusName_Failed,
  AssociationStatusName_Pending,
  AssociationStatusName_Success,
  AssociationStatusName'
  #-}
