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
-- Module      : Amazonka.FSx.Types.DeleteOpenZFSVolumeOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DeleteOpenZFSVolumeOption
  ( DeleteOpenZFSVolumeOption
      ( ..,
        DeleteOpenZFSVolumeOption_DELETE_CHILD_VOLUMES_AND_SNAPSHOTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeleteOpenZFSVolumeOption = DeleteOpenZFSVolumeOption'
  { fromDeleteOpenZFSVolumeOption ::
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

pattern DeleteOpenZFSVolumeOption_DELETE_CHILD_VOLUMES_AND_SNAPSHOTS :: DeleteOpenZFSVolumeOption
pattern DeleteOpenZFSVolumeOption_DELETE_CHILD_VOLUMES_AND_SNAPSHOTS = DeleteOpenZFSVolumeOption' "DELETE_CHILD_VOLUMES_AND_SNAPSHOTS"

{-# COMPLETE
  DeleteOpenZFSVolumeOption_DELETE_CHILD_VOLUMES_AND_SNAPSHOTS,
  DeleteOpenZFSVolumeOption'
  #-}
