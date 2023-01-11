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
-- Module      : Amazonka.OpsWorks.Types.LayerType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.LayerType
  ( LayerType
      ( ..,
        LayerType_Aws_flow_ruby,
        LayerType_Custom,
        LayerType_Db_master,
        LayerType_Ecs_cluster,
        LayerType_Java_app,
        LayerType_Lb,
        LayerType_Memcached,
        LayerType_Monitoring_master,
        LayerType_Nodejs_app,
        LayerType_Php_app,
        LayerType_Rails_app,
        LayerType_Web
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LayerType = LayerType'
  { fromLayerType ::
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

pattern LayerType_Aws_flow_ruby :: LayerType
pattern LayerType_Aws_flow_ruby = LayerType' "aws-flow-ruby"

pattern LayerType_Custom :: LayerType
pattern LayerType_Custom = LayerType' "custom"

pattern LayerType_Db_master :: LayerType
pattern LayerType_Db_master = LayerType' "db-master"

pattern LayerType_Ecs_cluster :: LayerType
pattern LayerType_Ecs_cluster = LayerType' "ecs-cluster"

pattern LayerType_Java_app :: LayerType
pattern LayerType_Java_app = LayerType' "java-app"

pattern LayerType_Lb :: LayerType
pattern LayerType_Lb = LayerType' "lb"

pattern LayerType_Memcached :: LayerType
pattern LayerType_Memcached = LayerType' "memcached"

pattern LayerType_Monitoring_master :: LayerType
pattern LayerType_Monitoring_master = LayerType' "monitoring-master"

pattern LayerType_Nodejs_app :: LayerType
pattern LayerType_Nodejs_app = LayerType' "nodejs-app"

pattern LayerType_Php_app :: LayerType
pattern LayerType_Php_app = LayerType' "php-app"

pattern LayerType_Rails_app :: LayerType
pattern LayerType_Rails_app = LayerType' "rails-app"

pattern LayerType_Web :: LayerType
pattern LayerType_Web = LayerType' "web"

{-# COMPLETE
  LayerType_Aws_flow_ruby,
  LayerType_Custom,
  LayerType_Db_master,
  LayerType_Ecs_cluster,
  LayerType_Java_app,
  LayerType_Lb,
  LayerType_Memcached,
  LayerType_Monitoring_master,
  LayerType_Nodejs_app,
  LayerType_Php_app,
  LayerType_Rails_app,
  LayerType_Web,
  LayerType'
  #-}
