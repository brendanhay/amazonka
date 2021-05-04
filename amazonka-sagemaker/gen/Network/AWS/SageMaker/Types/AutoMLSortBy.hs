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
-- Module      : Network.AWS.SageMaker.Types.AutoMLSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLSortBy
  ( AutoMLSortBy
      ( ..,
        AutoMLSortBy_CreationTime,
        AutoMLSortBy_Name,
        AutoMLSortBy_Status
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AutoMLSortBy = AutoMLSortBy'
  { fromAutoMLSortBy ::
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

pattern AutoMLSortBy_CreationTime :: AutoMLSortBy
pattern AutoMLSortBy_CreationTime = AutoMLSortBy' "CreationTime"

pattern AutoMLSortBy_Name :: AutoMLSortBy
pattern AutoMLSortBy_Name = AutoMLSortBy' "Name"

pattern AutoMLSortBy_Status :: AutoMLSortBy
pattern AutoMLSortBy_Status = AutoMLSortBy' "Status"

{-# COMPLETE
  AutoMLSortBy_CreationTime,
  AutoMLSortBy_Name,
  AutoMLSortBy_Status,
  AutoMLSortBy'
  #-}
