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
-- Module      : Network.AWS.SageMaker.Types.SortAssociationsBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SortAssociationsBy
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

import qualified Network.AWS.Core as Core

newtype SortAssociationsBy = SortAssociationsBy'
  { fromSortAssociationsBy ::
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
