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
-- Module      : Amazonka.SageMaker.Types.SortAssociationsBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SortAssociationsBy
  ( SortAssociationsBy
      ( ..,
        SortAssociationsBy_CreationTime,
        SortAssociationsBy_DestinationArn,
        SortAssociationsBy_DestinationType,
        SortAssociationsBy_SourceArn,
        SortAssociationsBy_SourceType
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SortAssociationsBy = SortAssociationsBy'
  { fromSortAssociationsBy ::
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

pattern SortAssociationsBy_CreationTime :: SortAssociationsBy
pattern SortAssociationsBy_CreationTime = SortAssociationsBy' "CreationTime"

pattern SortAssociationsBy_DestinationArn :: SortAssociationsBy
pattern SortAssociationsBy_DestinationArn = SortAssociationsBy' "DestinationArn"

pattern SortAssociationsBy_DestinationType :: SortAssociationsBy
pattern SortAssociationsBy_DestinationType = SortAssociationsBy' "DestinationType"

pattern SortAssociationsBy_SourceArn :: SortAssociationsBy
pattern SortAssociationsBy_SourceArn = SortAssociationsBy' "SourceArn"

pattern SortAssociationsBy_SourceType :: SortAssociationsBy
pattern SortAssociationsBy_SourceType = SortAssociationsBy' "SourceType"

{-# COMPLETE
  SortAssociationsBy_CreationTime,
  SortAssociationsBy_DestinationArn,
  SortAssociationsBy_DestinationType,
  SortAssociationsBy_SourceArn,
  SortAssociationsBy_SourceType,
  SortAssociationsBy'
  #-}
