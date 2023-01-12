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
-- Module      : Amazonka.Panorama.Types.NodeCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeCategory
  ( NodeCategory
      ( ..,
        NodeCategory_BUSINESS_LOGIC,
        NodeCategory_MEDIA_SINK,
        NodeCategory_MEDIA_SOURCE,
        NodeCategory_ML_MODEL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NodeCategory = NodeCategory'
  { fromNodeCategory ::
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

pattern NodeCategory_BUSINESS_LOGIC :: NodeCategory
pattern NodeCategory_BUSINESS_LOGIC = NodeCategory' "BUSINESS_LOGIC"

pattern NodeCategory_MEDIA_SINK :: NodeCategory
pattern NodeCategory_MEDIA_SINK = NodeCategory' "MEDIA_SINK"

pattern NodeCategory_MEDIA_SOURCE :: NodeCategory
pattern NodeCategory_MEDIA_SOURCE = NodeCategory' "MEDIA_SOURCE"

pattern NodeCategory_ML_MODEL :: NodeCategory
pattern NodeCategory_ML_MODEL = NodeCategory' "ML_MODEL"

{-# COMPLETE
  NodeCategory_BUSINESS_LOGIC,
  NodeCategory_MEDIA_SINK,
  NodeCategory_MEDIA_SOURCE,
  NodeCategory_ML_MODEL,
  NodeCategory'
  #-}
