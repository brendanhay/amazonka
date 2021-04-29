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
-- Module      : Network.AWS.EC2.Types.AssociationStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociationStatusCode
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype AssociationStatusCode = AssociationStatusCode'
  { fromAssociationStatusCode ::
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
