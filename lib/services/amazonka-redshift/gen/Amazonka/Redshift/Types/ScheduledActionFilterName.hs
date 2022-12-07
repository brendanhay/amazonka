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
-- Module      : Amazonka.Redshift.Types.ScheduledActionFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ScheduledActionFilterName
  ( ScheduledActionFilterName
      ( ..,
        ScheduledActionFilterName_Cluster_identifier,
        ScheduledActionFilterName_Iam_role
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype ScheduledActionFilterName = ScheduledActionFilterName'
  { fromScheduledActionFilterName ::
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

pattern ScheduledActionFilterName_Cluster_identifier :: ScheduledActionFilterName
pattern ScheduledActionFilterName_Cluster_identifier = ScheduledActionFilterName' "cluster-identifier"

pattern ScheduledActionFilterName_Iam_role :: ScheduledActionFilterName
pattern ScheduledActionFilterName_Iam_role = ScheduledActionFilterName' "iam-role"

{-# COMPLETE
  ScheduledActionFilterName_Cluster_identifier,
  ScheduledActionFilterName_Iam_role,
  ScheduledActionFilterName'
  #-}
