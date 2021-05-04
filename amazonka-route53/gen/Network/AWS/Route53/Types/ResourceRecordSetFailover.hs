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
-- Module      : Network.AWS.Route53.Types.ResourceRecordSetFailover
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResourceRecordSetFailover
  ( ResourceRecordSetFailover
      ( ..,
        ResourceRecordSetFailover_PRIMARY,
        ResourceRecordSetFailover_SECONDARY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

newtype ResourceRecordSetFailover = ResourceRecordSetFailover'
  { fromResourceRecordSetFailover ::
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

pattern ResourceRecordSetFailover_PRIMARY :: ResourceRecordSetFailover
pattern ResourceRecordSetFailover_PRIMARY = ResourceRecordSetFailover' "PRIMARY"

pattern ResourceRecordSetFailover_SECONDARY :: ResourceRecordSetFailover
pattern ResourceRecordSetFailover_SECONDARY = ResourceRecordSetFailover' "SECONDARY"

{-# COMPLETE
  ResourceRecordSetFailover_PRIMARY,
  ResourceRecordSetFailover_SECONDARY,
  ResourceRecordSetFailover'
  #-}
