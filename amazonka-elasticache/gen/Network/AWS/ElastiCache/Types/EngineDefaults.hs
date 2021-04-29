{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.EngineDefaults
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.EngineDefaults where

import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
import Network.AWS.ElastiCache.Types.Parameter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a @DescribeEngineDefaultParameters@ operation.
--
-- /See:/ 'newEngineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { -- | A list of parameters specific to a particular cache node type. Each
    -- element in the list contains detailed information about one parameter.
    cacheNodeTypeSpecificParameters :: Prelude.Maybe [CacheNodeTypeSpecificParameter],
    -- | Specifies the name of the cache parameter group family to which the
    -- engine default parameters apply.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
    -- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
    -- @redis6.x@ |
    cacheParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of engine default parameters.
    parameters :: Prelude.Maybe [Parameter],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EngineDefaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheNodeTypeSpecificParameters', 'engineDefaults_cacheNodeTypeSpecificParameters' - A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
--
-- 'cacheParameterGroupFamily', 'engineDefaults_cacheParameterGroupFamily' - Specifies the name of the cache parameter group family to which the
-- engine default parameters apply.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ |
--
-- 'parameters', 'engineDefaults_parameters' - Contains a list of engine default parameters.
--
-- 'marker', 'engineDefaults_marker' - Provides an identifier to allow retrieval of paginated results.
newEngineDefaults ::
  EngineDefaults
newEngineDefaults =
  EngineDefaults'
    { cacheNodeTypeSpecificParameters =
        Prelude.Nothing,
      cacheParameterGroupFamily = Prelude.Nothing,
      parameters = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
engineDefaults_cacheNodeTypeSpecificParameters :: Lens.Lens' EngineDefaults (Prelude.Maybe [CacheNodeTypeSpecificParameter])
engineDefaults_cacheNodeTypeSpecificParameters = Lens.lens (\EngineDefaults' {cacheNodeTypeSpecificParameters} -> cacheNodeTypeSpecificParameters) (\s@EngineDefaults' {} a -> s {cacheNodeTypeSpecificParameters = a} :: EngineDefaults) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the name of the cache parameter group family to which the
-- engine default parameters apply.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ |
engineDefaults_cacheParameterGroupFamily :: Lens.Lens' EngineDefaults (Prelude.Maybe Prelude.Text)
engineDefaults_cacheParameterGroupFamily = Lens.lens (\EngineDefaults' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@EngineDefaults' {} a -> s {cacheParameterGroupFamily = a} :: EngineDefaults)

-- | Contains a list of engine default parameters.
engineDefaults_parameters :: Lens.Lens' EngineDefaults (Prelude.Maybe [Parameter])
engineDefaults_parameters = Lens.lens (\EngineDefaults' {parameters} -> parameters) (\s@EngineDefaults' {} a -> s {parameters = a} :: EngineDefaults) Prelude.. Lens.mapping Prelude._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
engineDefaults_marker :: Lens.Lens' EngineDefaults (Prelude.Maybe Prelude.Text)
engineDefaults_marker = Lens.lens (\EngineDefaults' {marker} -> marker) (\s@EngineDefaults' {} a -> s {marker = a} :: EngineDefaults)

instance Prelude.FromXML EngineDefaults where
  parseXML x =
    EngineDefaults'
      Prelude.<$> ( x Prelude..@? "CacheNodeTypeSpecificParameters"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        ( Prelude.parseXMLList
                            "CacheNodeTypeSpecificParameter"
                        )
                  )
      Prelude.<*> (x Prelude..@? "CacheParameterGroupFamily")
      Prelude.<*> ( x Prelude..@? "Parameters"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "Parameter")
                  )
      Prelude.<*> (x Prelude..@? "Marker")

instance Prelude.Hashable EngineDefaults

instance Prelude.NFData EngineDefaults
