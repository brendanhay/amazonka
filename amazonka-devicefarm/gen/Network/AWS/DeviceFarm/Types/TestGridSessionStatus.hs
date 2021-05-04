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
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionStatus
  ( TestGridSessionStatus
      ( ..,
        TestGridSessionStatus_ACTIVE,
        TestGridSessionStatus_CLOSED,
        TestGridSessionStatus_ERRORED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TestGridSessionStatus = TestGridSessionStatus'
  { fromTestGridSessionStatus ::
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

pattern TestGridSessionStatus_ACTIVE :: TestGridSessionStatus
pattern TestGridSessionStatus_ACTIVE = TestGridSessionStatus' "ACTIVE"

pattern TestGridSessionStatus_CLOSED :: TestGridSessionStatus
pattern TestGridSessionStatus_CLOSED = TestGridSessionStatus' "CLOSED"

pattern TestGridSessionStatus_ERRORED :: TestGridSessionStatus
pattern TestGridSessionStatus_ERRORED = TestGridSessionStatus' "ERRORED"

{-# COMPLETE
  TestGridSessionStatus_ACTIVE,
  TestGridSessionStatus_CLOSED,
  TestGridSessionStatus_ERRORED,
  TestGridSessionStatus'
  #-}
