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
-- Module      : Amazonka.AuditManager.Types.ObjectTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ObjectTypeEnum
  ( ObjectTypeEnum
      ( ..,
        ObjectTypeEnum_ASSESSMENT,
        ObjectTypeEnum_ASSESSMENT_REPORT,
        ObjectTypeEnum_CONTROL,
        ObjectTypeEnum_CONTROL_SET,
        ObjectTypeEnum_DELEGATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ObjectTypeEnum = ObjectTypeEnum'
  { fromObjectTypeEnum ::
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

pattern ObjectTypeEnum_ASSESSMENT :: ObjectTypeEnum
pattern ObjectTypeEnum_ASSESSMENT = ObjectTypeEnum' "ASSESSMENT"

pattern ObjectTypeEnum_ASSESSMENT_REPORT :: ObjectTypeEnum
pattern ObjectTypeEnum_ASSESSMENT_REPORT = ObjectTypeEnum' "ASSESSMENT_REPORT"

pattern ObjectTypeEnum_CONTROL :: ObjectTypeEnum
pattern ObjectTypeEnum_CONTROL = ObjectTypeEnum' "CONTROL"

pattern ObjectTypeEnum_CONTROL_SET :: ObjectTypeEnum
pattern ObjectTypeEnum_CONTROL_SET = ObjectTypeEnum' "CONTROL_SET"

pattern ObjectTypeEnum_DELEGATION :: ObjectTypeEnum
pattern ObjectTypeEnum_DELEGATION = ObjectTypeEnum' "DELEGATION"

{-# COMPLETE
  ObjectTypeEnum_ASSESSMENT,
  ObjectTypeEnum_ASSESSMENT_REPORT,
  ObjectTypeEnum_CONTROL,
  ObjectTypeEnum_CONTROL_SET,
  ObjectTypeEnum_DELEGATION,
  ObjectTypeEnum'
  #-}
